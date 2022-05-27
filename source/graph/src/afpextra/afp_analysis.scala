/*  Author:     Fabian Huch, TU Munich

AFP entity graph analysis.
 */

package isabelle.afpextra


import isabelle.Sessions.Selection
import isabelle._
import isabelle.graph.Analysis.Score._
import isabelle.graph.Analysis.{Distribution, Scoring}
import isabelle.graph.Filter.Dir
import isabelle.graph.Graph.{Entity_Kind, Node_Kind, UId}
import isabelle.graph.Importer_Service.Import
import isabelle.graph._
import isabelle.graph.cypher.{Cypher_Analysis_Repository, Cypher_Graph_Repository, Neo4j_Data_Repository}
import isabelle.utils.Lines

import afp.AFP_Structure

import scala.collection.MapView

import org.neo4j.driver.AuthTokens


object AFP_Analysis
{
  sealed case class Aspect_Args(
    output_dir: Path,
    input_dir: Path,
    components: List[Component],
    dirs: List[Path] = Nil,
    progress: Progress)
  {
    def write_json(aspect: String, json: JSON.T): Unit =
      File.write(output_dir + Path.basic(aspect + ".json"), JSON.Format(json))
    def write_xml(aspect: String, xml: XML.Body): Unit =
      File.write(output_dir + Path.basic(aspect + ".xml"), XML.string_of_body(xml))
    def write_csv(aspect: String, header: List[String], records: List[CSV.Record]): Unit =
      CSV.File(aspect, header, records).write(output_dir)
  }

  sealed trait Aspect {
    def name: String
    def process(args: Aspect_Args): Unit
  }

  val lint_names = new Aspect {
    override def name: String = "lint_names"
    override def process(args: Aspect_Args): Unit =
    {
      val lint_file = args.input_dir + Path.basic("lints.json")
      val json = JSON.parse(File.read(lint_file)).asInstanceOf[Map[String, List[Map[String, JSON.T]]]]

      val theory_lints: List[(String, String)] = for {
        report <- json.getOrElse("reports", error("No reports"))
        theory = report("theory").asInstanceOf[String]
        lint <- report("report").asInstanceOf[Map[String,List[Map[String,JSON.T]]]]("results")
      } yield theory -> lint("name").asInstanceOf[String]
      val by_theory = theory_lints.groupBy(_._1).view.mapValues(_.map(_._2))

      val lines = for {
        component <- args.components
        session <- component.sessions
        theory <- session.theories
        lint <- by_theory.getOrElse(theory, Nil)
      } yield CSV.Record(component.name, session.name, theory, lint)

      args.write_csv(name, List("component", "session", "theory", "lint"), lines)
    }
  }

  val lint_count = new Aspect {
    override def name: String = "lint_count"
    override def process(args: Aspect_Args): Unit =
    {
      val lint_file = args.input_dir + Path.basic("lints.json")
      val json = JSON.parse(File.read(lint_file)).asInstanceOf[Map[String, List[Map[String, JSON.T]]]]

      val theory_lints: List[(String, String)] = for {
        report <- json.getOrElse("reports", error("No reports"))
        theory = report("theory").asInstanceOf[String]
        lint <- report("report").asInstanceOf[Map[String,List[Map[String,JSON.T]]]]("results")
      } yield theory -> lint("severity").asInstanceOf[String]
      val by_theory = theory_lints.groupBy(_._1).view.mapValues(_.groupBy(_._2).view.mapValues(_.size))

      val lines = for {
        component <- args.components
        session <- component.sessions
        theory <- session.theories
        lints <- by_theory.get(theory)
        counts = List("info","warn","error").map(lints.getOrElse(_, 0))
      } yield CSV.Record(component.name :: session.name :: theory :: counts: _*)

      args.write_csv(name, List("component", "session", "theory", "info", "warn", "error"), lines)
    }
  }


  val known_aspects = List(lint_count, lint_names)

  def the_aspect(name: String): Aspect =
    known_aspects.find(_.name == name) getOrElse
      error("Unknown aspect " + quote(name))


  /* analyze a session selection */

  case class Session(name: String, theories: List[String])
  case class Component(name: String, sessions: List[Session])

  def analysis(
    aspects: List[Aspect],
    components: List[Component],
    output_dir: Path,
    input_dir: Path,
    dirs: List[Path] = Nil,
    progress: Progress = new Progress): Unit =
  {
    Isabelle_System.make_directory(output_dir)
    val args = Aspect_Args(output_dir, input_dir, components, dirs, progress)
    aspects.foreach(_.process(args))
  }

  def exported_theories(session_name: String, deps: Sessions.Deps): List[String] =
  {
    val base = deps.get(session_name).getOrElse(error("No base for " + session_name))
    base.session_theories.map(_.theory)
  }


  /* Isabelle Tool Wrapper */

  val isabelle_tool: Isabelle_Tool = Isabelle_Tool("analysis", "Analyze", Scala_Project.here,
    args =>
  {
    var aspects: List[Aspect] = Nil
    var base_sessions: List[String] = Nil
    var select_dirs: List[Path] = Nil
    var input_dir: Path = Path.explode(".")
    var output_dir: Path = Path.explode("analysis")
    var requirements = false
    var exclude_session_groups: List[String] = Nil
    var all_sessions = false
    var dirs: List[Path] = Nil
    var session_groups: List[String] = Nil
    var exclude_sessions: List[String] = Nil

    val getopts = Getopts(
      """
Usage: isabelle analysis [OPTIONS] [SESSIONS...]

Options are:
-A NAMES     analyze named aspects
-B NAME      include session NAME and all descendants
-D DIR       include session directory and select its sessions
-I DIR       input dir (default: """ + input_dir.implode + """)
-O DIR       output dir (default: """ + output_dir.implode + """)
-R           refer to requirements of selected sessions
-X NAME      exclude sessions from group NAME and all descendants
-a           select all sessions
-d DIR       include session directory
-g NAME      select session group NAME
-x NAME      exclude session NAME and all descendants

Analyze.
""",
      "A:" -> (arg => aspects = Library.distinct(space_explode(',', arg)).map(the_aspect)),
      "B:" -> (arg => base_sessions = base_sessions ::: List(arg)),
      "D:" -> (arg => select_dirs = select_dirs ::: List(Path.explode(arg))),
      "I:" -> (arg => input_dir = Path.explode(arg)),
      "O:" -> (arg => output_dir = Path.explode(arg)),
      "R" -> (_ => requirements = true),
      "X:" -> (arg => exclude_session_groups = exclude_session_groups ::: List(arg)),
      "a" -> (_ => all_sessions = true),
      "d:" -> (arg => dirs = dirs ::: List(Path.explode(arg))),
      "g:" -> (arg => session_groups = session_groups ::: List(arg)),
      "x:" -> (arg => exclude_sessions = exclude_sessions ::: List(arg)))

    val sessions = getopts(args)
    val options = Options.init()
    val progress = new Console_Progress()

    val selection = Sessions.Selection(
      requirements = requirements,
      all_sessions = all_sessions,
      base_sessions = base_sessions,
      exclude_session_groups = exclude_session_groups,
      exclude_sessions = exclude_sessions,
      session_groups = session_groups,
      sessions = sessions)

    progress.interrupt_handler {
      val full_sessions =
        Sessions.load_structure(options = options, dirs = dirs, select_dirs = select_dirs)

      val sessions_structure = full_sessions.selection(selection)
      val deps = Sessions.deps(sessions_structure)

      val sessions = sessions_structure.build_selection(selection).toSet

      val components = sessions_structure.selection(selection).build_topological_order.filter(
        sessions.contains).map { session_name =>

        val theories = exported_theories(session_name, deps)

        Component(session_name, List(Session(session_name, theories)))
      }

      analysis(aspects = aspects, components = components, input_dir = input_dir,
        output_dir = output_dir, dirs = dirs, progress = progress)
    }
  })

  /* AFP Tool Wrapper */

  val afp_tool: Isabelle_Tool = Isabelle_Tool("afp_analysis", "Analyze afp", Scala_Project.here,
    args =>
  {
    var aspects: List[Aspect] = Nil
    var input_dir: Path = Path.explode(".")
    var output_dir: Path = Path.explode("analysis")
    var exclude_session_groups: List[String] = Nil
    var exclude_sessions: List[String] = Nil

    val getopts = Getopts(
      """
Usage: isabelle afp_graph_analysis [OPTIONS] GRAPHDB [ENTRIES]

Options are:
  -A NAMES     analyze named aspects
  -I DIR       input dir (default: """ + input_dir.implode + """)
  -O DIR       output dir (default: """ + output_dir + """)
  -X NAME      exclude sessions from group NAME and all descendants
  -x NAME      exclude session NAME and all descendants

Analyze afp theorem dependency graph.
""",
    "A:" -> (arg => aspects = Library.distinct(space_explode(',', arg)).map(the_aspect)),
    "I:" -> (arg => input_dir = Path.explode(arg)),
    "O:" -> (arg => output_dir = Path.explode(arg)),
    "X:" -> (arg => exclude_session_groups = exclude_session_groups ::: List(arg)),
    "x:" -> (arg => exclude_sessions = exclude_sessions ::: List(arg)))

    val entries = getopts(args)
    val options = Options.init()
    val progress = new Console_Progress()

    val base = Path.explode("$AFP_BASE")
    val thys = Path.explode("$AFP")

    val selection = Sessions.Selection(
      exclude_session_groups = exclude_session_groups,
      exclude_sessions = exclude_sessions,
      session_groups = List("AFP"))

    progress.interrupt_handler {
      val full_sessions =
        Sessions.load_structure(dirs = List(thys), options = options)

      val sessions_structure = full_sessions.selection(selection)
      val deps = Sessions.deps(sessions_structure)

      val structure = AFP_Structure(base)
      val selected_entries = if (entries.nonEmpty) entries else structure.entries
      val components = selected_entries.flatMap { entry =>
        val all_entry_sessions = structure.entry_sessions(entry).map(_.name)
        val selection = Sessions.Selection(
          exclude_session_groups = exclude_session_groups,
          exclude_sessions = exclude_sessions,
          sessions = all_entry_sessions)

        val entry_sessions = full_sessions.selection(selection).build_topological_order.filter(
          all_entry_sessions.contains).map { session_name =>

          val theories = exported_theories(session_name, deps)

          Session(session_name, theories)
        }
        if (entry_sessions.isEmpty) None else Some(Component(entry, entry_sessions))
      }

      analysis(
        aspects = aspects, components = components, input_dir = input_dir,
        output_dir = output_dir, dirs = List(thys), progress = progress)
    }
  })
}
