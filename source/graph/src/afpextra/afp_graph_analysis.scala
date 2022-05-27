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

import org.neo4j.driver.AuthTokens


object AFP_Graph_Analysis
{
  sealed case class Aspect_Args(
    name: String,
    output_dir: Path,
    input_dir: Path,
    session_names: List[String],
    theory_names: List[String],
    repo: Graph_Repository,
    analysis_repo: Analysis_Repository,
    dirs: List[Path] = Nil,
    progress: Progress)
  {
    val graph_service = new Graph_Service(repo)
    val service = new Analysis_Service(analysis_repo, repo)
    val selection = session_names.map(UId.apply)
    def write_json(aspect: String, json: JSON.T): Unit =
      File.write(mkdir(output_dir + Path.basic(aspect)) + Path.basic(name + ".json"), JSON.Format(json))
    def write_xml(aspect: String, xml: XML.Body): Unit =
      File.write(mkdir(output_dir + Path.basic(aspect)) + Path.basic(name + ".xml"), XML.string_of_body(xml))
    def write_csv(aspect: String, header: List[String], records: List[CSV.Record]): Unit =
      CSV.File(name, header, records).write(mkdir(output_dir + Path.basic(aspect)))
  }

  def mkdir(path: Path): Path = Isabelle_System.make_directory(path)

  sealed trait Aspect {
    def name: String
    def process(args: Aspect_Args): Unit
    def finish(output_dir: Path): Unit
  }

  val named_metrics = List(
    "eigenvector" -> Eigenvector_Centrality,
    "in_degree" -> Degree_Centrality(Dir.From),
    "closeness" -> Closeness_Centrality(),
    "betweenness" -> Betweenness_Centrality,
    "clustering" -> Clustering_Coefficient)

  /* session / theory sizes */

  def et_to_href(id: UId): String = id.value.split('.').toList match {
    case kind :: session :: res =>
      val entity = res.mkString(".")
      val theory = Long_Name.explode(entity).head
      "/theories/" + session.toLowerCase + "/#" + theory + ".html#" + entity + "|" + kind
    case _ => error("Invalid id format: " + id)
  }

  def et_to_href2(id: UId): String = id.value.split('.').toList match {
    case kind :: session :: res =>
      val entity = res.mkString(".")
      val theory = Long_Name.explode(entity).head
      "https://www.isa-afp.org/browser_info/current/AFP/" + session + "/" + theory + ".html#" + entity + "|" + kind
    case _ => error("Invalid id format: " + id)
  }

  def et_from_href2(href: String): UId = {
    val Entity = "([A-Za-z0-9_-]+)/([A-Za-z0-9_-]+)\\.html#([^|]+)\\|(.*)".r
    href.stripPrefix("https://www.isa-afp.org/browser_info/current/AFP/") match {
      case Entity(session, theory, entity, kind) =>
        UId.to_entity_uid(session, Entity_Kind.from_string(kind), entity)
      case _ => error("Malformed entity url: " + quote(href))
    }
  }

  def norm_score(score: Double): Double =
  {
    if (score.isNaN) Double.MinValue
    else if (score.isPosInfinity) Double.MaxValue
    else if (score.isNegInfinity) Double.MinValue
    else score
  }

  /* most central theorems */

  def filter_top_thms(args: Aspect_Args, sc: Scoring): LazyList[(UId, Double)] =
  {
    var seen_pos: Set[UId] = Set.empty
    val lazy_scores = LazyList.from(sc.node_scores.toSeq.sortBy(_._2).reverse)

    for {
      (id, score) <- lazy_scores

      entity <- args.graph_service.get_entity(id)
      if List(Entity_Kind.THM, Entity_Kind.AXIOM).contains(entity.kind)

      pos <- args.graph_service.position_of(id)
      if !seen_pos.contains(pos.uid)
      _ = seen_pos += pos.uid

      entities = args.graph_service.entities_of(pos.uid)

      if entities.forall(_.kind == Entity_Kind.THM)
    } yield entities.sortBy(_.name).minBy(_.name.length).uid -> norm_score(score)
  }


  /* most important concepts */

  def filter_top_concepts(args: Aspect_Args, sc: Scoring): LazyList[(UId, Double)] =
  {
    val kinds = List(Entity_Kind.CLASS, Entity_Kind.LOCALE, Entity_Kind.TYPE, Entity_Kind.CONST)
    var seen_pos: Set[UId] = Set.empty

    def as_t(id: UId): UId =
      UId(Entity_Kind.TYPE.toString + "." + id.value.split('.').drop(1).dropRight(1).mkString("."))

    val lazy_scores = LazyList.from(sc.node_scores.toSeq.sortBy(_._2).reverse)

    for {
      (id, score) <- lazy_scores

      entity <- args.graph_service.get_entity(id)
      if kinds.contains(entity.kind)

      pos <- args.graph_service.position_of(id)
      if !seen_pos.contains(pos.uid)

      _ = { seen_pos += pos.uid }

      shortest = args.graph_service.entities_of(pos.uid).filter(et =>
        kinds.contains(et.kind)).minBy(et => (et.name, kinds.indexOf(et.kind)))

      elem <- if (Entity_Kind.CONST != entity.kind) Some(shortest) else {
        args.graph_service.get_entity(as_t(entity.uid)) match {
          case Some(entity_t) =>
            val pos_t = args.graph_service.position_of(entity_t.uid).get
            if (!seen_pos.contains(pos_t.uid)) {
              seen_pos += pos_t.uid
              Some(entity_t)
            } else None
          case None => Some(shortest)
        }
      }
    } yield elem.uid -> norm_score(score)
  }


  /* graph */

  val graph = new Aspect {
    var ctxt = Map.empty[String, JSON.T]
    override def name: String = "graph"
    override def process(args: Aspect_Args): Unit =
    {
      val service = new Analysis_Service(args.analysis_repo, args.repo)
      val graph_service = new Graph_Service(args.repo)
      val selection = args.session_names.map(UId.apply)

      def thy_session(thy_id: UId): String = graph_service.session_of(thy_id).get.name

      def thy_name(thy_id: String) = Long_Name.base_name(thy_id)

      val graph = service.subgraph(Node_Kind.THEORY, selection)
      val num_et_by_id = service.score(Node_Kind.THEORY, selection, Count(Node_Kind.ENTITY))
        .node_scores
      val num_pos_by_id = service.score(Node_Kind.THEORY, selection, Count(Node_Kind.POSITION))
        .node_scores

      val max_ets = num_et_by_id.values.maxOption.getOrElse(0.0)
      val max_pos = num_pos_by_id.values.maxOption.getOrElse(0.0)

      val slocs_list = Lines.loc(
        Lines.Mode.SLOC,
        count_blobs = true,
        dirs = args.dirs,
        selection = Sessions.Selection(sessions = args.session_names),
        progress = args.progress)

      val max_slocs = slocs_list.map(_.lines).maxOption.getOrElse(0)
      val slocs = slocs_list.groupBy(_.session).map {
        case (session, counts) =>
          session -> JSON.Object(counts.map(count => thy_name(count.theory) -> count.lines): _*)
      }

      def sloc_by_id(id: UId): Int = slocs(thy_session(id))(thy_name(id.value)).asInstanceOf[Int]

      def thy_to_href(id: UId) =
        "/theories/" + thy_session(id).toLowerCase + "/#" + thy_name(id.value) + ".html"

      def entity_node(id: UId): JSON.T =
      {
        val session = thy_session(id)
        JSON.Object(
          "data" -> JSON.Object(
            "id" -> id.value,
            "name" -> thy_name(id.value),
            "url" -> thy_to_href(id),
            "parent" -> session,
            "sloc" -> sloc_by_id(id),
            "sloc_rel" -> sloc_by_id(id).toDouble / max_slocs,
            "num_et" -> num_et_by_id(id),
            "num_et_rel" -> num_et_by_id(id) / max_ets,
            "num_pos" -> num_pos_by_id(id),
            "num_pos_rel" -> num_pos_by_id(id) / max_pos,
            "et_by_sloc" -> num_et_by_id(id) / sloc_by_id(id),
            "et_by_pos" -> num_et_by_id(id) / num_pos_by_id(id),
            "pos_by_sloc" -> num_pos_by_id(id) / sloc_by_id(id)))
      }

      def session_node(session: String): JSON.T = JSON.Object("data" -> JSON.Object(
        "id" -> session,
        "name" -> session))

      val cytoscape_graph = JSON.Object(
        "nodes" -> (graph.nodes.toList.map(entity_node) ::: args.session_names.map(session_node)),
        "edges" -> graph.edges.filterNot(e => e._1 == e._3).toList.map(edge => JSON.Object(
          "data" -> JSON.Object(
            "id" -> (edge._1.value + "/" + edge._3.value),
            "source" -> edge._1.value,
            "count" -> edge._2,
            "count_rel" -> edge._2.toDouble / (num_et_by_id(edge._1) * num_et_by_id(edge._3)),
            "count_rel_2" -> Math
              .sqrt(edge._2.toDouble / (num_et_by_id(edge._1) * num_et_by_id(edge._3))),
            "target" -> edge._3.value))))

      val res = JSON.Object("elements" -> cytoscape_graph)
      args.write_json(name, res)
      ctxt += name -> res
    }
    override def finish(output_dir: Path): Unit = {
      val file = output_dir + Path.basic("graph.json")
      File.write(file, JSON.Format(ctxt))
    }
  }

  /* avg path length */

  val average_path_length = new Aspect {
    var ctxt: List[(String, Double)] = Nil
    override def name: String = "average_path_length"
    override def process(args: Aspect_Args): Unit =
    {
      val nodes = args.service.node(Node_Kind.ENTITY, args.selection)
      val avg_dist = args.analysis_repo.average_path_length(nodes)
      args.progress.echo(args.name + ": " + avg_dist)
      ctxt ::= args.name -> avg_dist
    }
    override def finish(output_dir: Path): Unit =
    {
      val lines = ctxt.map { case (name, value) => CSV.Record(name, value) }
      CSV.File(name, List("name", "avg_length"), lines).write(output_dir)
    }
  }

  val indegree_distribution = new Aspect {
    var args: Option[Aspect_Args] = None
    var ctxt: List[UId] = Nil
    override def name: String = "indegree_distribution"
    override def process(args: Aspect_Args): Unit =
    {
      this.args = Some(args)
      ctxt :::= args.session_names.map(UId.to_session_uid)
    }
    override def finish(output_dir: Path): Unit = args.foreach { args =>
      val res = args.service.degree_dist(Node_Kind.ENTITY, Nil, Dir.From)
      val lines = res.sorted.map(freq => CSV.Record(freq.value, freq.count)).toList
      CSV.File(name, List("k_in", "n"), lines).write(output_dir)
    }
  }

  /* prediction */

  val top_prediction = new Aspect {
    override def name: String = "top_prediction"
    override def process(args: Aspect_Args): Unit =
    {
      val n = 10
      val metrics = named_metrics.map(_._2)

      def map_scores(top_n: List[UId]): List[String] = top_n.map { id =>
        val entity = args.graph_service.get_entity(id).get
        val html = HTML.text(entity.name + " (" + entity.kind.toString + ") in ") :+
          HTML.link(et_to_href2(id), HTML.text(entity.uid.value.split('.').drop(2).head))
        XML.string_of_body(html)
      }

      val concepts = metrics.flatMap { metric =>
        val scoring = args.service.score(Node_Kind.ENTITY, args.selection, metric)
        map_scores(filter_top_concepts(args, scoring).take(n).toList.map(_._1))
      }

      val scorings =
        metrics.map(args.service.thm_score(args.selection, _)) ++
        metrics.map(args.service.score(Node_Kind.ENTITY, args.selection, _))

      val thms = map_scores(scorings.flatMap(filter_top_thms(args, _).take(n).toList).map(_._1))

      var html = ""
      if (concepts.nonEmpty) html +=
        "<h3>Important concepts</h3>" +
          "<ul>" +
          concepts.map(s => "<li>" + s + "</li>").mkString +
          "</ul>"

      if (thms.nonEmpty) html +=
        "<h3>Central theorems</h3>" +
          "<ul>" +
          thms.map(s => "<li>" + s + "</li>").mkString +
          "</ul>"
    }
    override def finish(output_dir: Path): Unit = {}
  }


  /* top concepts */

  val top_concepts = new Aspect {
    var ctxt = Map.empty[String, JSON.T]
    override def name: String = "top_concepts"
    override def process(args: Aspect_Args): Unit =
    {
      val n = 8
      val metric = Degree_Centrality(Dir.From)

      val scoring = args.service.score(Node_Kind.ENTITY, args.selection, metric)
      val top_n = filter_top_concepts(args, scoring).take(n).toList
      val max = top_n.headOption.map(_._2).getOrElse(0.0)
      val json = top_n.map { case (id, score) =>
        val entity = args.graph_service.get_entity(id).get
        JSON.Object(
          "entity" -> entity.name,
          "kind" -> entity.kind.toString,
          "url" -> et_to_href(id),
          "url2" -> et_to_href2(id),
          "score" -> score,
          "score_rel" -> score / max)
      }
      args.write_json(name, json)
      ctxt += args.name -> json
    }
    override def finish(output_dir: Path): Unit =
    {
      val file = output_dir + Path.basic("top_concepts.json")
      File.write(file, JSON.Format(ctxt))
    }
  }

  def csv_app_field(field: Any, records: List[CSV.Record]): List[CSV.Record] =
    records.map(e => CSV.Record(field +: e.fields: _*))

  val evaluation = new Aspect {
    var ctxt = Map.empty[String, List[CSV.Record]]
    val header = List("type", "metric", "pos", "num")
    override def name: String = "evaluation"
    override def process(args: Aspect_Args): Unit =
    {
      val results_file = args.input_dir + Path.basic("results.json")
      val json = JSON.parse(File.read(results_file)).asInstanceOf[Map[String, Map[String, JSON.T]]]
      val json_res = json.getOrElse(args.name, error("No results found for " + args.name))
      val concepts = json_res.getOrElse("concepts",
        error("Concepts missing for " + args.name)).asInstanceOf[List[String]].map(et_from_href2)
      val theorems = json_res.getOrElse("theorems",
        error("Theorems missing for " + args.name)).asInstanceOf[List[String]].map(et_from_href2)

      val concepts_pos = concepts.map(args.graph_service.position_of).distinct
      val theorems_pos = theorems.map(args.graph_service.position_of).distinct

      var res: Map[String, List[CSV.Record]] = Map.empty
      named_metrics.foreach { case (name, metric) =>
        val scoring = args.service.score(Node_Kind.ENTITY, args.selection, metric)
        val scoring_thm = args.service.thm_score(args.selection, metric)
        val top_concepts = filter_top_concepts(args, scoring).map(_._1)
        val top_theorems = filter_top_thms(args, scoring).map(_._1)
        val top_theorems_thm = filter_top_thms(args, scoring_thm).map(_._1)
        val top_concepts_pos = top_concepts.map(args.graph_service.position_of).distinct
        val top_theorems_pos = top_theorems.map(args.graph_service.position_of).distinct
        val top_theorems_thm_pos = top_theorems_thm.map(args.graph_service.position_of).distinct

        def add_csv(item: String, col: List[Int]): Unit =
        {
          val rows1 = col.map(i => CSV.Record(item, name, i, col.length))
          res = res.updatedWith(item)(rows0 => Some(rows0.getOrElse(Nil) ++ rows1))
        }

        add_csv("concept", concepts.map(top_concepts.indexOf))
        add_csv("concept_pos", concepts_pos.map(top_concepts_pos.indexOf))
        add_csv("theorems", theorems.map(top_theorems.indexOf))
        add_csv("theorems_pos", theorems_pos.map(top_theorems_pos.indexOf))
        add_csv("theorems_thm", theorems.map(top_theorems_thm.indexOf))
        add_csv("theorems_thm_pos", theorems_pos.map(top_theorems_thm_pos.indexOf))
      }

      val lines = res.values.toList.flatten
      args.write_csv(name, header, lines)
      ctxt += args.name -> lines
    }
    override def finish(output_dir: Path): Unit = {
      val lines = ctxt.toList.flatMap { case (name, rows) => csv_app_field(name, rows) }
      CSV.File("evaluation", "name" :: header,  lines).write(output_dir)
    }
  }

  val stats = new Aspect {
    var ctxt: List[CSV.Record] = Nil
    override def name: String = "stats"
    override def process(args: Aspect_Args): Unit =
    {
      val stats = args.service.stats(Node_Kind.ENTITY, args.selection)

      val slocs = Lines.loc(
        Lines.Mode.SLOC,
        count_blobs = false,
        dirs = args.dirs,
        selection = Sessions.Selection(sessions = args.session_names),
        progress = args.progress)

      ctxt ::= CSV.Record(args.name, stats.nodes, stats.edges, slocs.map(_.lines).sum)
    }
    override def finish(output_dir: Path): Unit =
    {
      val header = List("name", "nodes", "edges", "sloc")
      CSV.File(name, header, ctxt).write(output_dir)
    }
  }

  val theory_metrics = new Aspect {
    var ctxt = Map.empty[String, List[CSV.Record]]
    val header = "theory" :: "nodes" :: "edges" :: "out_degree" :: named_metrics.map(_._1)
    override def name: String = "theory_metrics"
    override def process(args: Aspect_Args): Unit =
    {
      val scores: List[Map[UId, Double]] = (Degree_Centrality(Dir.To)::named_metrics.map(_._2)).map(
        args.service.score(Node_Kind.THEORY, args.selection, _).node_scores)

      val slocs = Lines.loc(
        Lines.Mode.SLOC,
        count_blobs = false,
        dirs = args.dirs,
        selection = Sessions.Selection(sessions = args.session_names),
        progress = args.progress)

      val lines = slocs.map { sloc =>
        val id = UId.to_theory_uid(sloc.theory)
        val stats = args.service.theory_stats(Node_Kind.ENTITY, Seq(id))

        CSV.Record(sloc.theory :: stats.nodes :: stats.edges :: scores.map(_.getOrElse(id, Double.NaN)): _*)
      }

      args.write_csv(name, header, lines)
      ctxt += args.name -> lines
    }
    override def finish(output_dir: Path): Unit =
    {
      val lines = ctxt.toList.flatMap { case (name, lines) => csv_app_field(name, lines) }
      CSV.File(name, "name" :: header, lines).write(output_dir)
    }
  }

  val pos_metrics = new Aspect {
    var ctxt = Map.empty[String, List[CSV.Record]]
    val header = "theory" :: "pos" :: "out_degree" :: named_metrics.map(_._1)
    override def name: String = "pos_metrics"
    override def process(args: Aspect_Args): Unit =
    {
      def thy(id: UId): String = id.value.split('.').dropRight(1).mkString(".")
      def pos(id: UId): String = id.value.split('.').takeRight(1).mkString(".")

      def score_by_thy(sc: Scoring): Map[String, Map[String, Double]] =
        sc.node_scores.groupBy(e => thy(e._1)).view.mapValues(_.map(e => pos(e._1) -> e._2).toMap).toMap

      val scores: List[Map[String, Map[String, Double]]] = (Degree_Centrality(Dir.To)::named_metrics.map(_._2)).map(
        args.service.score(Node_Kind.POSITION, args.selection, _)).map(score_by_thy)

      val slocs = Lines.loc(
        Lines.Mode.SLOC,
        count_blobs = false,
        dirs = args.dirs,
        selection = Sessions.Selection(sessions = args.session_names),
        progress = args.progress)

      val lines = slocs.flatMap { sloc =>
        scores.head.get(sloc.theory) match {
          case Some(value) => value.keys.map { pos =>
            CSV.Record(sloc.theory :: pos :: scores.map(_ (sloc.theory).getOrElse(pos, Double.NaN)): _*)
          }
          case None => Nil
        }
      }

      args.write_csv(name, header, lines)
      ctxt += args.name -> lines
    }
    override def finish(output_dir: Path): Unit =
    {
      val lines = ctxt.toList.flatMap { case (name, lines) => csv_app_field(name, lines) }
      CSV.File(name, "name" :: header, lines).write(output_dir)
    }
  }

  val entity_metrics = new Aspect {
    var ctxt: List[CSV.Record] = Nil
    val header = "theory" :: "out_degree" :: named_metrics.map(_._1)
    override def name: String = "entity_metrics"
    override def process(args: Aspect_Args): Unit =
    {
      def thy(id: UId): String = id.value.split('.').slice(1, 3).mkString(".")
      val scores = (Degree_Centrality(Dir.To)::named_metrics.map(_._2)).map { score =>
        args.service.score(Node_Kind.ENTITY, args.selection, score).node_scores
      }

      val lines = scores.head.keys.toList.map(e =>
        CSV.Record(thy(e) :: scores.map(_.getOrElse(e, Double.NaN)):_*))

      args.write_csv(name, header, lines)
      ctxt ++= lines
    }
    override def finish(output_dir: Path): Unit =
      CSV.File(name, header, ctxt).write(output_dir)
  }

  val detail_stats = new Aspect {
    var ctxt = Map.empty[String, List[CSV.Record]]
    val header = "theory" :: "sloc" :: "entities" :: "out_degree" :: named_metrics.map(_._1)
    override def name: String = "detail_stats"
    override def process(args: Aspect_Args): Unit =
    {
      def thy(id: UId): String = id.value.split('.').slice(1, 3).mkString(".")

      def sizes_by_thy(sc: Scoring): Map[String, Int] =
        sc.node_scores.groupBy(e => thy(e._1)).view.mapValues(_.size).toMap

      def avg_by_thy(sc: Scoring): Map[String, Double] =
        sc.node_scores.groupBy(e => thy(e._1)).view.mapValues(e => e.values.sum / e.size).toMap

      val scores = (Degree_Centrality(Dir.To)::named_metrics.map(_._2)).map(
        args.service.score(Node_Kind.ENTITY, args.selection, _))

      val sizes = sizes_by_thy(scores.head)

      val avg_scores = scores.map(avg_by_thy)

      val slocs = Lines.loc(
        Lines.Mode.SLOC,
        count_blobs = false,
        dirs = args.dirs,
        selection = Sessions.Selection(sessions = args.session_names),
        progress = args.progress)

      val lines = slocs.map { sloc =>
        CSV.Record(sloc.theory :: sloc.lines :: sizes.getOrElse(sloc.theory, 0) ::  avg_scores.map(_.getOrElse(sloc.theory, Double.NaN)): _*)
      }

      args.write_csv(name, header, lines)
      ctxt += args.name -> lines
    }
    override def finish(output_dir: Path): Unit =
    {
      val lines = ctxt.toList.flatMap { case (name, lines) => csv_app_field(name, lines) }
      CSV.File(name, "name" :: header, lines).write(output_dir)
    }
  }

  val degree_distribution = new Aspect {
    var ctxt = Map.empty[String, Distribution]
    var ctxt1 = Map.empty[Int, Long]
    val header = List("degree", "count")
    override def name: String = "degree_distribution"
    override def process(args: Aspect_Args): Unit =
    {
      val dist = args.service.degree_dist(Node_Kind.ENTITY, args.selection, Dir.From)
      val lines = dist.sorted.map(freq => CSV.Record(freq.value.toInt, freq.count)).toList
      args.write_csv(name, header, lines)
      ctxt += args.name -> dist
      ctxt1 ++= dist.sorted.map(e => e.value.toInt -> e.count)
    }
    override def finish(output_dir: Path): Unit =
    {
      val lines1 = ctxt1.toList.groupBy(_._1).view.mapValues(_.map(_._2).sum).toList.sorted
      CSV.File(name, header, lines1.map(e => CSV.Record(e._1, e._2))).write(output_dir)
      val lines = ctxt.toList.flatMap { case (name, dist) =>
        csv_app_field(name, dist.sorted.map(e => CSV.Record(e.value.toInt, e.count)).toList)
      }
      CSV.File(name + "_individual", "name" :: header, lines).write(output_dir)
    }
  }

  val metrics = new Aspect {
    val header = named_metrics.map(_._1)
    var ctxt = Map.empty[String, List[CSV.Record]]
    override def name: String = "metrics"
    override def process(args: Aspect_Args): Unit =
    {
      val metric_values = named_metrics.map(_._2).map(
        args.service.score(Node_Kind.ENTITY, args.selection, _))
      val lines = metric_values.head.node_scores.keys.toList.map { key =>
        CSV.Record(metric_values.map(_.node_scores(key)): _*)
      }
      ctxt += args.name -> lines
      args.write_csv(name, header, lines)
    }
    override def finish(output_dir: Path): Unit =
    {
      val lines = ctxt.flatMap { case (name, lines) => csv_app_field(name, lines) }
      CSV.File(name, "name" :: header, lines.toList).write(output_dir)
    }
  }

  val fractal_dim = new Aspect {
    var ctxt: List[CSV.Record] = Nil
    val header = List("name", "l_b", "n_b")
    override def name: String = "fractal_dimension"
    override def process(args: Aspect_Args): Unit =
    {
      val sel_dim = args.analysis_repo.fractal_dimension(
        args.name, args.service.node(Node_Kind.ENTITY, args.selection))
      val lines = sel_dim.map { case (l, n) => CSV.Record(args.name, l, n)}
      ctxt ++= lines
      args.write_csv(name, header, lines)
    }
    override def finish(output_dir: Path): Unit =
      CSV.File(name, header, ctxt).write(output_dir)
  }

  val theory_fractal_dim = new Aspect {
    var ctxt = Map.empty[String, List[CSV.Record]]
    val header = List("theory", "l_b", "n_b")
    override def name: String = "theory_fractal_dimension"
    override def process(args: Aspect_Args): Unit =
    {
      val lines = args.theory_names.flatMap { theory =>
        val id = UId.to_theory_uid(theory)
        val f_ds = args.service.theory_fractal_dim(Node_Kind.ENTITY, Seq(id))
        f_ds.map { case (l, n) => CSV.Record(theory, l, n) }
      }

      ctxt += args.name -> lines
      args.write_csv(name, header, lines)
    }
    override def finish(output_dir: Path): Unit =
    {
      val lines = ctxt.toList.flatMap { case (name, lines) => csv_app_field(name, lines) }
      CSV.File(name, "name" :: header, lines).write(output_dir)
    }
  }

  val known_aspects = List(graph, top_prediction, top_concepts, evaluation, stats, detail_stats,
    degree_distribution, metrics, theory_metrics, pos_metrics, entity_metrics, fractal_dim,
    average_path_length, indegree_distribution, theory_fractal_dim)

  def the_aspect(name: String): Aspect =
    known_aspects.find(_.name == name) getOrElse
      error("Unknown aspect " + quote(name))


  /* import a session */

  def import_sessions(
    imports: List[Import], repo: Graph_Repository,
    progress: Progress = new Progress): Unit =
  {
    progress.echo("importing " + imports.size + " sessions with " +
      imports.flatMap(_.theory_names).size + " theories...")
    try {
      val service = new Importer_Service(repo)
      service.import_sessions(imports)
      progress.echo("finished importing")
    } catch {
      case e: Exception => progress.echo_error_message(e.getMessage + "\n" +
        e.getStackTrace.mkString("\n"))
    }
  }


  /* analyze a session selection */

  case class Component(name: String, imports: List[Import])

  def graph_analysis(
    aspects: List[Aspect],
    components: List[Component],
    graph_repo: Graph_Repository,
    analysis_repo: Analysis_Repository,
    output_dir: Path,
    input_dir: Path,
    skip_import: Boolean = false,
    fully_connected: Boolean = false,
    dirs: List[Path] = Nil,
    progress: Progress = new Progress): Unit =
  {
    mkdir(output_dir)
    if (!skip_import && fully_connected) {
      progress.echo("Building full graph...")
      import_sessions(components.flatMap(_.imports), graph_repo, progress)
    }

    components.foreach { component =>
      progress.echo("Processing " + component.name + " (" + component.imports.size + " sessions)")

      if (!skip_import && !fully_connected) import_sessions(component.imports, graph_repo, progress)

      val sessions = component.imports.map(_.session_name)
      val theories = component.imports.flatMap(_.theory_names)

      val args = Aspect_Args(component.name, output_dir, input_dir, sessions, theories, graph_repo,
        analysis_repo, dirs, progress)
      aspects.foreach(_.process(args))
    }
    aspects.foreach(_.finish(output_dir))
  }


  /* Isabelle Tool Wrapper */

  def exported_theories(
    session_name: String, deps: Sessions.Deps,
    store: Sessions.Store): List[String] =
  {
    val base = deps.get(session_name).getOrElse(error("No base for " + session_name))
    using(store.open_database_context()) { db_context =>
      base.session_theories.map(_.theory).filter { theory_name =>
        db_context.read_export(
          List(session_name), theory_name,
          Export.THEORY_PREFIX + "parents").isDefined
      }
    }
  }

  val isabelle_tool: Isabelle_Tool = Isabelle_Tool("graph_analysis", "Analyze dependency graph", Scala_Project.here,
    args =>
  {
    val build_options = Word.explode(Isabelle_System.getenv("ISABELLE_BUILD_OPTIONS"))

    var aspects: List[Aspect] = Nil
    var base_sessions: List[String] = Nil
    var fully_connected = false
    var select_dirs: List[Path] = Nil
    var input_dir: Path = Path.explode(".")
    var numa_shuffling = false
    var output_dir: Path = Path.explode("analysis")
    var requirements = false
    var verbose_build = false
    var exclude_session_groups: List[String] = Nil
    var all_sessions = false
    var count_blobs = false
    var clean_build = false
    var dirs: List[Path] = Nil
    var session_groups: List[String] = Nil
    var max_jobs = 1
    var n = 10
    var options = Options.init(opts = build_options) + "export_theory"
    var skip_import = false
    var exclude_sessions: List[String] = Nil

    val getopts = Getopts(
      """
Usage: isabelle graph_analysis [OPTIONS] GRAPHDB [SESSIONS...]

Options are:
-A NAMES     analyze named aspects
-B NAME      include session NAME and all descendants
-C           build graphs connected between sessions
-D DIR       include session directory and select its sessions
-I DIR       input dir (default: """ + input_dir.implode + """)
-N           cyclic shuffling of NUMA CPU nodes (performance tuning)
-O DIR       output dir (default: """ + output_dir.implode + """)
-R           refer to requirements of selected sessions
-V           verbose build
-X NAME      exclude sessions from group NAME and all descendants
-a           select all sessions
-c           clean build
-d DIR       include session directory
-g NAME      select session group NAME
-j INT       maximum number of parallel jobs (default 1)
-o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)
-s           skip import
-x NAME      exclude session NAME and all descendants

Analyze theorem dependency graph.
""",
      "A:" -> (arg => aspects = Library.distinct(space_explode(',', arg)).map(the_aspect)),
      "B:" -> (arg => base_sessions = base_sessions ::: List(arg)),
      "C" -> (_ => fully_connected = true),
      "D:" -> (arg => select_dirs = select_dirs ::: List(Path.explode(arg))),
      "I:" -> (arg => input_dir = Path.explode(arg)),
      "N" -> (_ => numa_shuffling = true),
      "O:" -> (arg => output_dir = Path.explode(arg)),
      "R" -> (_ => requirements = true),
      "V" -> (_ => verbose_build = true),
      "X:" -> (arg => exclude_session_groups = exclude_session_groups ::: List(arg)),
      "a" -> (_ => all_sessions = true),
      "b" -> (_ => count_blobs = true),
      "c" -> (_ => clean_build = true),
      "d:" -> (arg => dirs = dirs ::: List(Path.explode(arg))),
      "g:" -> (arg => session_groups = session_groups ::: List(arg)),
      "j:" -> (arg => max_jobs = Value.Int.parse(arg)),
      "n:" -> (arg => n = Value.Int.parse(arg)),
      "o:" -> (arg => options = options + arg),
      "s" -> (_ => skip_import = true),
      "x:" -> (arg => exclude_sessions = exclude_sessions ::: List(arg)))

    val (graphdb, sessions) = getopts(args) match {
      case graphdb :: sessions => (graphdb, sessions)
      case _ => getopts.usage()
    }

    val neo4j = Neo4j_Data_Repository.remote(graphdb, AuthTokens.basic("neo4j", "admin"))

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
      val res =
        Build.build(
          options = options,
          selection = selection,
          progress = progress,
          clean_build = clean_build,
          dirs = dirs,
          select_dirs = select_dirs,
          numa_shuffling = NUMA.enabled_warning(progress, numa_shuffling),
          max_jobs = max_jobs,
          verbose = verbose_build)
      if (!res.ok) System.exit(res.rc)

      val store = Sessions.store(options)
      val cache = XML.Cache.make()

      val library_sessions = Sessions.load_structure(options).selection(
        Selection(all_sessions = true)).imports_topological_order.toSet
      val full_sessions =
        Sessions.load_structure(options = options, dirs = dirs, select_dirs = select_dirs)

      val sessions_structure = full_sessions.selection(selection)
      val deps = Sessions.deps(sessions_structure)

      val sessions = sessions_structure.build_selection(selection).toSet

      val imports = sessions_structure.selection(selection).build_topological_order.filter(
        sessions.contains).filter(_ != "Pure").map { session_name =>

        val is_library = library_sessions.contains(session_name)
        val theories = exported_theories(session_name, deps, store)

        def provider = Export.Provider.database(
          store.open_database(session_name), cache, session_name, "dummy")

        Import(is_library, session_name, () => provider, theories)
      }
      val components = imports.map(i => Component(i.session_name, List(i)))

      using(Cypher_Graph_Repository(neo4j)) { graph_repo =>
        val analysis_repo = Cypher_Analysis_Repository(neo4j)
        graph_analysis(aspects = aspects, components = components, graph_repo = graph_repo,
          analysis_repo = analysis_repo, input_dir = input_dir, output_dir = output_dir,
          skip_import = skip_import, fully_connected = fully_connected, dirs = dirs,
          progress = progress)
      }
    }
  })

  /* AFP Tool Wrapper */

  val afp_tool: Isabelle_Tool = Isabelle_Tool("afp_graph_analysis", "Analyze afp dependency graph", Scala_Project.here,
    args =>
  {
    val build_options = Word.explode(Isabelle_System.getenv("ISABELLE_BUILD_OPTIONS"))

    var aspects: List[Aspect] = Nil
    var fully_connected = false
    var input_dir: Path = Path.explode(".")
    var numa_shuffling = false
    var output_dir: Path = Path.explode("analysis")
    var verbose_build = false
    var exclude_session_groups: List[String] = Nil
    var clean_build = false
    var max_jobs = 1
    var n = 10
    var options = Options.init(opts = build_options) + "export_theory"
    var skip_import = false
    var verbose = false
    var exclude_sessions: List[String] = Nil

    val getopts = Getopts(
      """
Usage: isabelle afp_graph_analysis [OPTIONS] GRAPHDB [ENTRIES]

Options are:
  -A NAMES     analyze named aspects
  -C           build graphs connected between sessions
  -I DIR       input dir (default: """ + input_dir.implode + """)
  -N           cyclic shuffling of NUMA CPU nodes (performance tuning)
  -O DIR       output dir (default: """ + output_dir + """)
  -V           verbose build
  -X NAME      exclude sessions from group NAME and all descendants
  -c           clean build
  -j INT       maximum number of parallel jobs (default 1)
  -n INT       number of top theorems and concepts
  -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)
  -s           skip import
  -v           verbose
  -x NAME      exclude session NAME and all descendants

Analyze afp theorem dependency graph.
""",
    "A:" -> (arg => aspects = Library.distinct(space_explode(',', arg)).map(the_aspect)),
    "C" -> (_ => fully_connected = true),
    "I:" -> (arg => input_dir = Path.explode(arg)),
    "N" -> (_ => numa_shuffling = true),
    "O:" -> (arg => output_dir = Path.explode(arg)),
    "V" -> (_ => verbose_build = true),
    "X:" -> (arg => exclude_session_groups = exclude_session_groups ::: List(arg)),
    "c" -> (_ => clean_build = true),
    "j:" -> (arg => max_jobs = Value.Int.parse(arg)),
    "n:" -> (arg => n = Value.Int.parse(arg)),
    "o:" -> (arg => options = options + arg),
    "s" -> (_ => skip_import = true),
    "v" -> (_ => verbose = true),
    "x:" -> (arg => exclude_sessions = exclude_sessions ::: List(arg)))

    val more_args = getopts(args)

    val (graphdb, entries) = more_args match {
      case graphdb :: entries => (graphdb, entries)
      case _ => getopts.usage()
    }

    val progress = new Console_Progress()

    val base = Path.explode("$AFP_BASE")
    val thys = Path.explode("$AFP")

    val selection = Sessions.Selection(
      exclude_session_groups = exclude_session_groups,
      exclude_sessions = exclude_sessions,
      session_groups = List("AFP"))

    progress.interrupt_handler {
      val res =
        Build.build(
          options = options,
          selection = selection,
          progress = progress,
          clean_build = clean_build,
          dirs = List(thys),
          numa_shuffling = NUMA.enabled_warning(progress, numa_shuffling),
          max_jobs = max_jobs,
          verbose = verbose_build)
      if (!res.ok) System.exit(res.rc)

      val store = Sessions.store(options)
      val cache = XML.Cache.make()

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
          def provider =
            Export.Provider
              .database(store.open_database(session_name), cache, session_name, "dummy")

          val theories = exported_theories(session_name, deps, store)

          Import(library = false, session_name, () => provider, theories)
        }
        if (entry_sessions.isEmpty) None else Some(Component(entry, entry_sessions))
      }

      val neo4j = Neo4j_Data_Repository.remote(graphdb, AuthTokens.basic("neo4j", "admin"))
      using(Cypher_Graph_Repository(neo4j)) { repo =>
        val analysis_repo = Cypher_Analysis_Repository(neo4j)

        graph_analysis(aspects = aspects, components = components, graph_repo = repo,
          analysis_repo = analysis_repo, input_dir = input_dir, output_dir = output_dir,
          skip_import = skip_import, fully_connected = fully_connected, dirs = List(thys),
          progress = progress)
      }
    }
  })
}
