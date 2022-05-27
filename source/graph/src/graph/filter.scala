/* Query filter.
 */
package isabelle.graph


import isabelle.graph.Graph.{Edge_Label, Fields, Node_Kind}


object Filter
{
  /* Query primitives */

  sealed trait Value {}

  object Value
  {
    case class String_Value(value: String) extends Value

    case class Int_Value(value: Int) extends Value

    case class Double_Value(value: Double) extends Value

    case class Boolean_Value(value: Boolean) extends Value
  }


  /* Single nodes filter */

  sealed trait Prop[+A <: Value] {}

  object Prop
  {
    case class And[A <: Value](p1: Prop[A], p2: Prop[A]) extends Prop[A]

    case class Not[A <: Value](p: Or[A]) extends Prop[A]

    case class Or[A <: Value](values: List[A]) extends Prop[A]
  }

  case class Field_Prop[+A <: Value](field: Fields.Value, prop: Prop[A])


  /* Isolated nodes / edges */

  case class Node_Props(kind: Option[Node_Kind.Value], fields: List[Field_Prop[Value]])

  case class Edge_Props(label: Option[Edge_Label.Value], directed: Boolean)


  /* Object graph */

  sealed trait Dir {}

  object Dir
  {
    case object To extends Dir

    case object From extends Dir

    case object Both extends Dir

    case object Any extends Dir
  }

  case class Edge(label: Option[Edge_Label.Value], dir: Dir, node: Node)

  case class Node(kind: Option[Node_Kind.Value], fields: List[Field_Prop[Value]], edges: List[Edge])

  case class Linear_Connection(
    kind: Option[Node_Kind.Value],
    fields: List[Field_Prop[Value]],
    label: Option[Edge_Label.Value],
    dir: Dir
  )

  case class Linear_Node(nodes: List[Linear_Connection])
}
