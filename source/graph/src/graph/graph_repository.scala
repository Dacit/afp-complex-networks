package isabelle.graph


import isabelle.graph.Graph._


trait Graph_Repository extends AutoCloseable
{
  /* Create */

  def save_sessions(sessions: Seq[Session_Node]): Unit

  def save_session(session: Session_Node): Unit = save_sessions(Seq(session))

  def save_theories(theories: Seq[Theory_Node]): Unit

  def save_theory(theory: Theory_Node): Unit = save_theories(Seq(theory))

  def save_positions(theory: UId, nodes: Map[Pos_Node, Seq[Entity_Node]]): Unit

  def save_edges(from: Node_Kind.Value, to: Node_Kind.Value, edges: Seq[Edge]): Unit


  /* Read */

  def get_entities(filter: Filter.Node): Seq[Entity_Node]

  def get_positions(filter: Filter.Node): Seq[Pos_Node]

  def get_theories(filter: Filter.Node): Seq[Theory_Node]

  def get_sessions(filter: Filter.Node): Seq[Session_Node]

  def list_sessions: Seq[Session_Node]

  def list_sessions(dist: Boolean): Seq[Session_Node]

  def list_theories(sessions: Seq[UId]): Seq[Theory_Node]

  def session_deps: Seq[Edge]


  /* Delete */

  def delete_session(session: UId): Unit = delete_sessions(Seq(session))

  def delete_sessions(sessions: Seq[UId]): Unit

  def delete_theory(theory: UId): Unit = delete_theories(Seq(theory))

  def delete_theories(theories: Seq[UId]): Unit


  /* Additional */

  def commit(): Unit
}
