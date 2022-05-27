/* Cypher impl for filters.
 */
package isabelle.graph.cypher


import isabelle.graph.Filter._
import isabelle.graph.Graph.{Edge_Label, Fields}


object Cypher_Filter
{


  /* Names */

  case class Name(name: String)
  {
    require(name.matches("^[a-zA-Z0-9_]+$"))

    override def toString: String = name
  }

  private def param_name(values: List[Value]): String =
  {
    val hash = values.hashCode()
    if (hash < 0) {
      "pn" + (-values.hashCode()).toString
    } else {
      "p" + values.hashCode().toString
    }
  }

  private def fresh_node_name(topo_name: Name): Name =
  {
    val digits = topo_name.toString.reverse.takeWhile(_.isDigit)
    val before = topo_name.toString.dropRight(digits.length)
    if (digits.nonEmpty) {
      Name(before + (Integer.parseInt(digits) + 1))
    } else {
      Name(before + "1")
    }
  }


  /* Properties */

  case class Prop_Query(query: String, params: Map[String, List[Value]])

  private def field_props_query[A <: Value](name: Name, field: Fields.Value, prop: Prop[A]): Prop_Query = prop match {
    case Prop.And(p1, p2) =>
      val q1 = field_props_query(name, field, p1)
      val q2 = field_props_query(name, field, p2)
      Prop_Query(s"(${ q1.query }) AND (${ q2.query })", q1.params ++ q2.params)
    case Prop.Not(prop) =>
      val q = field_props_query(name, field, prop)
      Prop_Query(s"NOT (${ q.query })", q.params)
    case Prop.Or(values) =>
      val param = param_name(values)
      Prop_Query(s"${ name }.${ field } IN $$${ param }", List(param -> values).toMap)
  }

  def field_prop_query[A <: Value](name: Name, field_prop: Field_Prop[A]): Prop_Query =
    field_props_query(name, field_prop.field, field_prop.prop)

  def edge_props_query(name: Option[Name], props: Edge_Props): String =
  {
    s"-[${ name.getOrElse("") }${ props.label.map(l => s":${ l }").getOrElse("") }]-${
      if (props.directed) ">" else ""
    }"
  }

  def linear_connection_query(name: Name, node: Linear_Connection): Props_Query =
  {
    val edge_fq = connection_query(None, node.label, node.dir)
    val node_fq = node_props_fq(name, Node_Props(node.kind, node.fields))
    node_fq.copy(query = node_fq.query + edge_fq)
  }

  def linear_node_query(name: Name, nodes: Linear_Node): Option[Props_Query] =
  {
    nodes.nodes.foldLeft((name, None: Option[Props_Query])) {
      case ((name0, props), node) =>
        val node_fq = linear_connection_query(name0, node)
        val props1 = props match {
          case None => Some(node_fq)
          case Some(props0) => Some(
            Props_Query(
              props0.query + node_fq.query,
              props0.clauses ++ node_fq.clauses,
              props0.params ++ node_fq.params
            )
          )
        }
        (fresh_node_name(name0), props1)
    }._2
  }

  private def connection_query(name: Option[Name], label: Option[Edge_Label.Value], dir: Dir) =
  {
    dir match {
      case Dir.To => edge_props_query(name, Edge_Props(label, directed = true))
      case Dir.From => s"<${ edge_props_query(name, Edge_Props(label, directed = false)) }"
      case Dir.Both => s"<${ edge_props_query(name, Edge_Props(label, directed = true)) }"
      case Dir.Any => edge_props_query(name, Edge_Props(label, directed = false))
    }
  }

  case class Props_Query(query: String, clauses: List[String], params: Map[String, List[Value]])

  def node_props_fq(name: Name, node_props: Node_Props): Props_Query =
  {
    val qs: List[Prop_Query] = node_props.fields.map(field_prop_query(name, _))
    val query = node_props.kind match {
      case Some(kind) => s"(${ name }:${ kind })"
      case None => s"(${ name })"
    }
    Props_Query(query, qs.map(_.query), qs.map(_.params).fold(Map.empty)(_ ++ _))
  }


  /* Entities */

  case class Entity_Query(queries: List[String], clauses: List[String], params: Map[String, List[Value]])

  private case class Topological_Order(linear_chain: List[Edge], additional_edges: List[(Node, Edge)])

  // Linearize graph from the given node (excluding node itself)
  private def linearize_graph(node: Node, visited: Set[Node] = Set.empty): Topological_Order =
  {
    // pick next
    node.edges.find(e => !visited.contains(e.node)) match {
      case Some(next) =>
        val res = linearize_graph(next.node, visited + node)
        val linear = next :: res.linear_chain
        val additional = node.edges.filter(_ != next).map(e => (node, e)) ++ res.additional_edges
        Topological_Order(linear, additional)
      case None =>
        Topological_Order(Nil, node.edges.map(e => (node, e)))
    }
  }

  private def graph_entity_query(
    root_name: Name,
    root_query: Props_Query,
    topo: Topological_Order
  ): Entity_Query =
  {
    val (chain_props_query, _, names) = topo.linear_chain
      .foldLeft((root_query, root_name, Map.empty[Node, Name])) {
        case ((fq, name, names), edge_node) =>
          val dir = edge_node.dir
          val edge_q = connection_query(None, edge_node.label, dir)
          val node_name = fresh_node_name(name)
          val node_q = node_props_fq(
            node_name, Node_Props(
              edge_node.node.kind, edge_node.node.fields))

          val query = s"${ fq.query }${ edge_q }${ node_q.query }"
          val clauses = fq.clauses ++ node_q.clauses
          val params = fq.params ++ node_q.params
          (Props_Query(query, clauses, params), node_name, names + (edge_node.node -> node_name))
      }
    val additional_queries = topo.additional_edges.map {
      case (from, e_to) => s"(${ names(from) })${
        connection_query(
          None, e_to.label, e_to.dir)
      }(${ names(e_to.node) })"
    }
    Entity_Query(
      chain_props_query.query +: additional_queries, chain_props_query.clauses, chain_props_query
        .params)
  }

  def edge_fq(edge_name: Name, edge: Edge): Entity_Query =
  {
    val node_name = fresh_node_name(edge_name)
    val root_node = node_props_fq(node_name, Node_Props(edge.node.kind, edge.node.fields))
    val edge_q =
      Props_Query(
        s"${ connection_query(Some(edge_name), edge.label, edge.dir) }${ root_node.query }",
        root_node.clauses,
        root_node.params
      )
    graph_entity_query(node_name, edge_q, linearize_graph(edge.node))
  }

  def node_fq(root_name: Name, root_node: Node): Entity_Query =
  {
    val node_q = node_props_fq(root_name, Node_Props(root_node.kind, root_node.fields))
    graph_entity_query(root_name, node_q, linearize_graph(root_node))
  }
}
