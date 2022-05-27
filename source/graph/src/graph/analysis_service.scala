package isabelle.graph


import isabelle.error
import isabelle.graph.Analysis.Score._
import isabelle.graph.Analysis._
import isabelle.graph.Filter.Value.String_Value
import isabelle.graph.Filter._
import isabelle.graph.Graph.Edge_Label.IN
import isabelle.graph.Graph.Fields.{ID, POS}
import isabelle.graph.Graph.Node_Kind.{ENTITY, POSITION, SESSION, THEORY}
import isabelle.graph.Graph.{Edge_Label, Entity_Kind, Fields, Node_Kind, UId}

import scala.collection.mutable


class Analysis_Service(analysis_repo: Analysis_Repository, graph_repo: Graph_Repository)
{
  private def linear_node(from: Node_Kind.Value, to: Node_Kind.Value = ENTITY): Linear_Node =
  {
    val value_list = List(SESSION, THEORY, POSITION, ENTITY)
    val right = value_list.length - 1 - value_list.indexOf(to)
    val between = value_list.dropWhile(_ != from).drop(1).dropRight(right)
    Linear_Node(between.reverse.map(kind => Linear_Connection(Some(kind), Nil, Some(IN), Dir.To)))
  }

  private def theories_node(sessions: Node): Node =
    Node(Some(THEORY), Nil, List(Edge(Some(IN), Dir.To, sessions)))

  private def position_node(theories: Node): Node =
    Node(Some(POSITION), Nil, List(Edge(Some(IN), Dir.To, theories)))

  private def entity_node(positions: Node): Node =
    Node(Some(ENTITY), Nil, List(Edge(Some(IN), Dir.To, positions)))

  def node(kind: Node_Kind.Value, sessions: Seq[UId]): Node = kind match {
    case SESSION =>
      val selection = if (sessions.nonEmpty) sessions else graph_repo.list_sessions.map(_.uid)
      val session_props: List[Field_Prop[Value]] =
        List(Field_Prop(ID, Prop.Or(selection.map(s => String_Value(s.value)).toList)))
      Node(Some(SESSION), session_props, Nil)
    case THEORY => theories_node(node(Node_Kind.SESSION, sessions))
    case POSITION => position_node(node(Node_Kind.THEORY, sessions))
    case ENTITY => entity_node(node(Node_Kind.POSITION, sessions))
  }

  def theory_node(kind: Node_Kind.Value, theories: Seq[UId]): Node = kind match {
    case SESSION => error("Invalid node kind: " + kind)
    case THEORY =>
      val theory_props: List[Field_Prop[Value]] =
      List(Field_Prop(ID, Prop.Or(theories.map(s => String_Value(s.value)).toList)))
      Node(Some(THEORY), theory_props, Nil)
    case POSITION => position_node(theory_node(Node_Kind.THEORY, theories))
    case ENTITY => entity_node(theory_node(Node_Kind.POSITION, theories))
  }

  def stats(kind: Node_Kind.Value, sessions: Seq[UId], directed: Boolean = true): Stats =
  {
    val nodes = node(kind, sessions)
    val merged_nodes = linear_node(kind).nodes.foldRight(nodes) {
      case (Linear_Connection(kind, fields, label, dir), node1) => Node(kind, fields, List(Edge(label, dir, node1)))
    }
    Stats(
      analysis_repo.count_nodes(nodes), analysis_repo
        .count_edges(merged_nodes, Edge_Props(None, directed = directed)))
  }

  def theory_stats(kind: Node_Kind.Value, theories: Seq[UId]): Stats =
  {
    val nodes = theory_node(kind, theories)
    val merged_nodes = linear_node(kind).nodes.foldRight(nodes) {
      case (Linear_Connection(kind, fields, label, dir), node1) => Node(kind, fields, List(Edge(label, dir, node1)))
    }
    Stats(analysis_repo.count_nodes(nodes),
      analysis_repo.count_edges(merged_nodes, Edge_Props(None, directed = true)))
  }

  def theory_fractal_dim(kind: Node_Kind.Value, theories: Seq[UId]): List[(Int, Int)] =
  {
    val nodes = theory_node(kind, theories)
    analysis_repo.fractal_dimension(theories.map(_.toString).mkString, nodes)
  }

  def degree_dist(kind: Node_Kind.Value, sessions: Seq[UId], dir: Dir): Distribution =
  {
    val nodes = node(kind, sessions)
    val linear_nodes = linear_node(kind)
    val edges = Edge_Props(None, directed = true)
    Distribution(analysis_repo.score_frequency(nodes, linear_nodes, edges, Degree_Centrality(dir)))
  }

  private def directed(score: Score): Boolean = score match {
    case Score.Clustering_Coefficient | Score.Effective_Size => false
    case _ => true
  }

  def score(kind: Node_Kind.Value, sessions: Seq[UId], score: Score): Scoring =
  {
    val nodes = node(kind, sessions)
    val linear_nodes = linear_node(kind)
    val edge = Edge_Props(None, directed(score))
    score match {
      case Count(kind1) =>
        analysis_repo.score(nodes, linear_node(kind, kind1), edge, score)
      case _ =>
        analysis_repo.score(nodes, linear_nodes, edge, score)
    }
  }

  def thm_score(sessions: Seq[UId], score: Score): Scoring =
  {
    val kind_thm = Field_Prop(Fields.KIND, Filter.Prop.Or(List(
      String_Value(Entity_Kind.THM.toString), String_Value(Entity_Kind.AXIOM.toString))))
    val to_pos = Filter.Edge(Some(Edge_Label.IN), Dir.To, node(Node_Kind.POSITION, sessions))
    val thy_subgraph_nodes = Filter.Node(Some(Node_Kind.ENTITY), List(kind_thm), List(to_pos))
    val thy_subgraph_ln = Linear_Node(Nil)
    val thy_subgraph_ep = Edge_Props(None, directed = true)

    analysis_repo.score(thy_subgraph_nodes, thy_subgraph_ln, thy_subgraph_ep, score)
  }

  def subgraph(kind: Node_Kind.Value, sessions: Seq[UId]): Graph =
  {
    val props = Edge_Props(None, directed = true)
    analysis_repo.subgraph(node(kind, sessions), linear_node(kind), props)
  }

  /* Impl */

  def afp_session_wccs: Seq[Component] =
  {
    val afp_sessions = graph_repo.list_sessions(dist = false).map(_.uid)
    val afp_deps = graph_repo.session_deps
      .filter(dep => afp_sessions.contains(dep.from) && afp_sessions.contains(dep.to))

    val builder = mutable.Map.newBuilder[UId, mutable.Set[UId]]
    builder ++= afp_sessions.map(s => s -> mutable.Set(s))
    val wccs: mutable.Map[UId, mutable.Set[UId]] = builder.result()

    afp_deps.foreach { e =>
      val from = wccs.getOrElseUpdate(e.from, mutable.Set(e.from))
      val to = wccs.get(e.to).map(_.toIterable).getOrElse(Iterable(e.to))
      // merge
      from ++= to
      to.foreach(t => wccs.update(t, from))
    }
    wccs.values.toSeq.distinct.map(s => Component(s.toSeq))
  }
}