/* Author: Fabian Huch, TU Muenchen

Line count utility based on markup.
 */

package isabelle.utils


import isabelle._


object Lines {

  case class Count(session: String, theory: String, lines: Int)

  object Mode extends Enumeration
  {
    /* physical lines */
    val LOC = Value("loc")

    /* non-blank lines */
    val NBLOC = Value("nbloc")

    /* source lines of code */
    val SLOC = Value("sloc")
  }

  def is_comment(kind: String): Boolean =
    List(
      Thy_Header.CHAPTER, Thy_Header.SECTION, Thy_Header.SUBSECTION, Thy_Header.SUBSUBSECTION,
      Thy_Header.PARAGRAPH, Thy_Header.SUBPARAGRAPH, Thy_Header.TEXT, Thy_Header.TXT,
      Thy_Header.TEXT_RAW).contains(kind)

  def source(tree: XML.Tree): String = tree match {
    case XML.Elem(Markup(Markup.ML_COMMENT, _), _) => ""
    case XML.Elem(Markup(Markup.COMMENT, _), _) => ""
    case XML.Elem(Markup.Command_Span(kind), _) if is_comment(kind) => ""
    case XML.Elem(_, body) => source(body)
    case XML.Text(content) => content
  }
  def source(body: XML.Body): String = body.map(source).mkString

  def loc(
    mode: Mode.Value,
    count_blobs: Boolean,
    options: Options = Options.init(),
    selection: Sessions.Selection = Sessions.Selection.empty,
    progress: Progress = new Progress,
    dirs: List[Path] = Nil,
    select_dirs: List[Path] = Nil,
    verbose: Boolean = false): List[Count] =
  {
    val store = Sessions.store(options)

    val full_sessions =
      Sessions.load_structure(options = options, dirs = dirs, select_dirs = select_dirs)

    val sessions_structure = full_sessions.selection(selection)
    val deps = Sessions.deps(sessions_structure)

    def theory_loc(
      db_context: Sessions.Database_Context,
      session: String,
      theory: String): Int =
    {
      def read(name: String): Export.Entry =
        db_context.get_export(List(session), theory, name)

      def read_xml(name: String): XML.Body =
        YXML.parse_body(
          Symbol.output(unicode_symbols = false, UTF8.decode_permissive(read(name).uncompressed)),
          cache = db_context.cache)

      def count(body: XML.Body): Int = mode match {
        case Mode.LOC =>
          split_lines(XML.content(body)).size
        case Mode.NBLOC =>
          split_lines(XML.content(body)).count(line => !line.isBlank)
        case Mode.SLOC =>
          split_lines(source(body)).count(line => !line.isBlank)
      }

      val thy_count = count(read_xml(Export.MARKUP))

      split_lines(read(Export.FILES).text) match {
        case thy_file :: blob_files =>
          progress.echo_if(verbose, "  File " + thy_file + ": " + thy_count)
          if (count_blobs) {
            thy_count + blob_files.zipWithIndex.map {
              case (blob_file, i) =>
                val blob = read_xml(Export.MARKUP + (i + 1))
                val blob_count = count(blob)
                progress.echo_if(verbose, "  Blob file " + blob_file + ": " + blob_count)
                blob_count
            }.sum
          }
          else thy_count
        case _ => thy_count
      }
    }

    sessions_structure.build_selection(selection).map(session_name => Future.fork {
      val base = deps.get(session_name).getOrElse(error("No base for " + session_name))
      val theories = base.session_theories.map(_.theory)

      val res = using(store.open_database_context()) { db_context =>
        theories.map(theory =>
          Count(session_name, theory, theory_loc(db_context, session_name, theory)))
      }
      progress.echo_if(verbose, "Session " + session_name + ": " + res.map(_.lines).sum)
      res
    }).flatMap(_.join)
  }

  /* Isabelle tool wrapper */

  val isabelle_tool = Isabelle_Tool("lines", "count lines", Scala_Project.here, args =>
  {
    val build_options = Word.explode(Isabelle_System.getenv("ISABELLE_BUILD_OPTIONS"))

    var base_sessions: List[String] = Nil
    var select_dirs: List[Path] = Nil
    var mode: String = Mode.SLOC.toString
    var numa_shuffling = false
    var output_file: Option[Path] = None
    var requirements = false
    var verbose_build = false
    var exclude_session_groups: List[String] = Nil
    var all_sessions = false
    var count_blobs = false
    var clean_build = false
    var dirs: List[Path] = Nil
    var session_groups: List[String] = Nil
    var max_jobs = 1
    var options = Options.init(opts = build_options)
    var verbose = false
    var exclude_sessions: List[String] = Nil

    val getopts = Getopts("""
Usage: isabelle lines [OPTIONS] [SESSIONS ...]

  Options are:
  -B NAME      include session NAME and all descendants
  -D DIR       include session directory and select its sessions
  -M MODE      counting methods: """ + commas_quote(Mode.values.map(_.toString)) + """
               (default: """ + mode + """)
  -N           cyclic shuffling of NUMA CPU nodes (performance tuning)
  -O FILE      output file
  -R           refer to requirements of selected sessions
  -V           verbose build
  -X NAME      exclude sessions from group NAME and all descendants
  -a           select all sessions
  -b           count blobs
  -c           clean build
  -d DIR       include session directory
  -g NAME      select session group NAME
  -j INT       maximum number of parallel jobs (default 1)
  -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)
  -v           verbose
  -x NAME      exclude session NAME and all descendants

Count lines of code.
""",
      "B:" -> (arg => base_sessions = base_sessions ::: List(arg)),
      "D:" -> (arg => select_dirs = select_dirs ::: List(Path.explode(arg))),
      "N" -> (_ => numa_shuffling = true),
      "M:" -> (arg => mode = arg),
      "O:" -> (arg => output_file = Some(Path.explode(arg))),
      "R" -> (_ => requirements = true),
      "V" -> (_ => verbose_build = true),
      "X:" -> (arg => exclude_session_groups = exclude_session_groups ::: List(arg)),
      "a" -> (_ => all_sessions = true),
      "b" -> (_ => count_blobs = true),
      "c" -> (_ => clean_build = true),
      "d:" -> (arg => dirs = dirs ::: List(Path.explode(arg))),
      "g:" -> (arg => session_groups = session_groups ::: List(arg)),
      "j:" -> (arg => max_jobs = Value.Int.parse(arg)),
      "o:" -> (arg => options = options + arg),
      "v" -> (_ => verbose = true),
      "x:" -> (arg => exclude_sessions = exclude_sessions ::: List(arg)))

    val mode0 = Mode.values.find(_.toString == mode).getOrElse(getopts.usage())

    val sessions = getopts(args)

    val progress = new Console_Progress(verbose = verbose_build)

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

      val counts = loc(
        mode = mode0,
        count_blobs = count_blobs,
        options = options,
        selection = selection,
        progress = progress,
        dirs = dirs,
        select_dirs = select_dirs,
        verbose = verbose)

      output_file.foreach { path =>
        val json = counts.map(count => JSON.Object.apply(
          "theory" -> count.theory,
          mode -> count.lines
        ))
        File.write(path, JSON.Format(json))
      }

      progress.echo(counts.map(_.lines).sum.toString)
    }
  })
}
