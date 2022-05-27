/* Author: Fabian Huch, TU Muenchen

Import theory export into dependency graph.
 */

package isabelle.graph


import isabelle.Export.Provider
import isabelle.Export_Theory.{Entity, Theory}
import isabelle.Markup.OFFSET
import isabelle.graph.Graph.UId.{to_entity_uid, to_position_uid, to_session_uid, to_theory_uid}
import isabelle.graph.Graph._
import isabelle.graph.Importer_Service.Import
import isabelle.{Console_Progress, Export_Theory, Progress, Properties}


/** Importer service
 *
 * @param repo underlying repository
 * @param progress logger
 */
class Importer_Service(repo: Graph_Repository, progress: Progress = new Console_Progress())
{

  /* Helpers */

  private def to_short_name(identifier: String, long_name: String): String =
  {
    long_name.stripPrefix(identifier + ".")
  }


  /* Mappers */

  private def to_session_node(session_name: String, library: Boolean): Session_Node =
  {
    Session_Node(to_session_uid(session_name), session_name, library)
  }

  private def to_theory_node(session: Session_Node, theory_name: String): Theory_Node =
  {
    Theory_Node(to_theory_uid(theory_name), to_short_name(session.name, theory_name))
  }

  private def to_position_entity_nodes(
    session_name: String,
    theory: Theory_Node,
    entities: Iterator[Entity[_]]): Map[Pos_Node, Seq[Entity_Node]] =
  {
    entities
      .filter(e => Entity_Kind.is_value(e.export_kind))
      .toSeq
      .groupBy(entity => Properties.get(entity.pos, OFFSET).get.toInt)
      .map {
        case (pos, entities) =>
          val entity_nodes = entities.map { entity =>
            val entity_id = to_entity_uid(session_name, Entity_Kind.from_string(entity.export_kind), entity.name)
            val entity_name = to_short_name(theory.name, entity.name)
            Entity_Node(entity_id, entity_name, Entity_Kind.from_string(entity.export_kind))
          }
          val pos_id = to_position_uid(theory.uid.value, pos)
          val pos_name = entity_nodes.minBy(_.name.length).name
          Pos_Node(pos_id, pos_name, pos) -> entity_nodes
      }
  }

  private def to_session_edge(session_uid: UId, theory_uid: UId): Edge =
  {
    Edge(theory_uid, session_uid, Edge_Label.IN)
  }

  private def to_parent_edges(theory_uid: UId, parents_names: List[String]): Seq[Edge] =
  {
    parents_names.map(parent_name => Edge(theory_uid, to_theory_uid(parent_name), Edge_Label.DEP))
  }

  /** Adds a theory into the repository. Overwrites existing theory with same name in that session.
   *
   * @param session_name to import into
   * @param theory       to import
   */
  def append_theory(session_name: String, theory: Theory): Unit =
  {
    // clean up old
    repo.delete_theory(to_theory_uid(theory.name))

    // Add session node if non exists
    val session_node = repo.list_sessions.find(_.name == session_name).getOrElse {
      val session = to_session_node(session_name, library = false)
      repo.save_session(session)
      session
    }

    // Add theory node
    val theory_node = to_theory_node(session_node, theory.name)
    repo.save_theories(Seq(theory_node))
    val edge_importer: Edge_Importer = ???

    // Add content nodes
    val position_entity_nodes = to_position_entity_nodes(
      session_name, theory_node, theory.entity_iterator)
    repo.save_positions(theory_node.uid, position_entity_nodes)

    // Add theory edges
    val structure_edge = to_session_edge(session_node.uid, theory_node.uid)
    repo.save_edges(Node_Kind.THEORY, Node_Kind.SESSION, Seq(structure_edge))
    val parent_edges = to_parent_edges(theory_node.uid, theory.parents)
    repo.save_edges(Node_Kind.THEORY, Node_Kind.THEORY, parent_edges)

    // Add content edges
    val entity_edges = edge_importer.to_edges(session_name, theory)
    repo.save_edges(Node_Kind.ENTITY, Node_Kind.ENTITY, entity_edges)
  }

  /** Imports sessions into the repository. Overwrites old data.
   *
   * @param sessions sessions with contained theories (names)
   */
  def import_sessions(sessions: List[Import]): Unit =
  {
    // Clean up old theories
    progress.echo("Cleaning up old content...")
    repo.delete_sessions(sessions.map(_.session_name).map(to_session_uid))

    // Add nodes
    val edge_importer_builder = new Edge_Importer.Builder()
    progress.echo("Adding structure and nodes...")
    for {
      session_structure <- sessions
      session_node = to_session_node(session_structure.session_name, session_structure.library)
      _ = repo.save_session(session_node)

      theory_name <- session_structure.theory_names
    } {
      val theory_node = to_theory_node(session_node, theory_name)
      repo.save_theory(theory_node)

      val provider = session_structure.provider()
      val theory = Export_Theory
        .read_theory(provider.focus(theory_name), session_structure.session_name, theory_name)
      edge_importer_builder.add_thy_deps(theory_name, theory.parents)

      progress.echo("Adding nodes for " + theory_name)
      val position_entity_nodes = to_position_entity_nodes(session_structure.session_name,
        theory_node, theory.entity_iterator)
      edge_importer_builder.add_entities(theory_name,
        theory.entity_iterator.filter(e => Entity_Kind.is_value(e.export_kind)).map(e => (Entity_Kind.from_string(e.export_kind), e.name)))
      repo.save_positions(theory_node.uid, position_entity_nodes)
      repo.commit()
    }
    progress.echo("Finalizing nodes...")

    // Prepare edge importer
    val edge_importer = edge_importer_builder.build()

    // Add edges
    progress.echo("Adding edges...")
    for {
      session_structure <- sessions
      theory_name <- session_structure.theory_names
    } {
      val provider = session_structure.provider()
      val theory = Export_Theory
        .read_theory(provider.focus(theory_name), session_structure.session_name, theory_name)
      progress.echo(s"Adding edges for ${ theory_name }")

      // Add theory edges
      val structure_edge = to_session_edge(
        to_session_uid(session_structure.session_name), to_theory_uid(theory_name))
      repo.save_edges(Node_Kind.THEORY, Node_Kind.SESSION, Seq(structure_edge))
      val parent_edges = to_parent_edges(to_theory_uid(theory_name), theory.parents)
      repo.save_edges(Node_Kind.THEORY, Node_Kind.THEORY, parent_edges)

      // Add content edges
      val entity_edges = edge_importer.to_edges(session_structure.session_name, theory)
      repo.save_edges(Node_Kind.ENTITY, Node_Kind.ENTITY, entity_edges)
      repo.commit()
    }
  }
}

object Importer_Service
{
  case class Import(
    library: Boolean,
    session_name: String,
    provider: () => Provider,
    theory_names: List[String])
}
