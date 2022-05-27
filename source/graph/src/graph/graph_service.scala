package isabelle.graph


import isabelle.{commas_quote, error}
import isabelle.graph.Filter.Value.{Boolean_Value, String_Value}
import isabelle.graph.Filter._
import isabelle.graph.Graph.Edge_Label.IN
import isabelle.graph.Graph.Fields.{DISTRIBUTION, ID}
import isabelle.graph.Graph.Node_Kind._
import isabelle.graph.Graph.{Entity_Kind, Entity_Node, Fields, Node_Kind, Pos_Node, Session_Node, Theory_Node, UId}


class Graph_Service(repo: Graph_Repository)
{
  def get_entity(id: UId): Option[Entity_Node] =
  {
    val e = Node(Some(Node_Kind.ENTITY), List(Field_Prop(Fields.ID, Prop.Or(List(String_Value(id.value))))), Nil)
    val res = repo.get_entities(e)
    res.headOption
  }

  def get_position(id: UId): Option[Pos_Node] =
  {
    val e = Node(Some(Node_Kind.POSITION), List(Field_Prop(Fields.ID, Prop.Or(List(String_Value(id.value))))), Nil)
    val res = repo.get_positions(e)
    res.headOption
  }

  def position_of(entity: UId): Option[Pos_Node] =
  {
    val e = Node(Some(ENTITY), List(Field_Prop(ID, Prop.Or(List(String_Value(entity.value))))), Nil)
    val p = Node(Some(POSITION), Nil, List(Edge(Some(IN), Dir.From, e)))
    val res = repo.get_positions(p).toList
    if (res.length > 1) error("Pos of " + entity + " returned multiple elems:" + commas_quote(res.map(_.label)))
    res.headOption
  }

  def entities_of(position: UId): Seq[Entity_Node] =
  {
    val p = Node(Some(Node_Kind.POSITION), List(Field_Prop(Fields.ID, Prop.Or(List(String_Value(position.value))))), Nil)
    val e = Node(Some(Node_Kind.ENTITY), Nil, List(Edge(Some(IN), Dir.To, p)))
    repo.get_entities(e)
  }

  def theory_of(pos: UId): Option[Theory_Node] =
  {
    val p = Node(Some(POSITION), List(Field_Prop(ID, Prop.Or(List(String_Value(pos.value))))), Nil)
    val t = Node(Some(THEORY), Nil, List(Edge(Some(IN), Dir.From, p)))
    val res = repo.get_theories(t).toList
    if (res.length > 1) error("Thy of " + pos + " returned multiple elems:" + commas_quote(res.map(_.name)))
    res.headOption
  }

  def session_of(theory: UId): Option[Session_Node] =
  {
    val t = Node(Some(THEORY), List(Field_Prop(ID, Prop.Or(List(String_Value(theory.value))))), Nil)
    val s = Node(Some(SESSION), Nil, List(Edge(Some(IN), Dir.From, t)))
    val res = repo.get_sessions(s).toList
    if (res.length > 1) error("Session of " + theory + " returned multiple elems:" + commas_quote(res.map(_.name)))
    res.headOption
  }

  def dist_sessions: Seq[UId] =
  {
    val s = Node(
      Some(SESSION), List(Field_Prop(
        DISTRIBUTION, Prop.Or(List(Boolean_Value(true))))), Nil)
    repo.get_sessions(s).map(_.uid)
  }
}

object Graph_Service
{
  def apply(repo: Graph_Repository): Graph_Service = new Graph_Service(repo)
}
