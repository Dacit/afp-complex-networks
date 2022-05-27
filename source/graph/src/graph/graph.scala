package isabelle.graph


import isabelle.error


object Graph
{
  object Entity_Kind extends Enumeration
  {
    val TYPE = Value("type")
    val CONST = Value("const")
    val THM = Value("thm")
    val AXIOM = Value("axiom")
    val LOCALE = Value("locale")
    val CLASS = Value("class")
    val LOCALE_DEPENDENCY = Value("locale_dependency")

    def is_value(value: String): Boolean = values.exists(_.toString == value)
    def from_string(value: String): Value = values.find(_.toString == value) getOrElse
      error("No entity: " + value)
  }

  object Fields extends Enumeration
  {
    val DISTRIBUTION = Value("dist")
    val ID = Value("id")
    val LABEL = Value("label")
    val KIND = Value("kind")
    val SESSION = Value("session")
    val THEORY = Value("theory")
    val COUNT = Value("count")
    val WEIGHT = Value("weight")
    val POS = Value("pos")
  }

  object Node_Kind extends Enumeration
  {
    val SESSION = Value("Session")
    val THEORY = Value("Theory")
    val POSITION = Value("Position")
    val ENTITY = Value("Entity")
  }

  object Edge_Label extends Enumeration
  {
    val IN = Value("in")
    val DEF = Value("def")
    val DEP = Value("dep")
    val REL = Value("rel")

    def from_string(value: String): Value = values.find(_.toString == value) getOrElse
      error("No edge: " + value)
  }

  case class UId(value: String)

  object UId
  {
    def to_session_uid(session_name: String): UId = UId(session_name)

    def to_theory_uid(theory_name: String): UId = UId(theory_name)

    def to_position_uid(theory: String, position: Long): UId = UId(theory + "." + position)

    def to_entity_uid(session: String, kind: Entity_Kind.Value, entity_name: String): UId =
      UId(kind.toString + "." + session + "." + entity_name)
  }

  case class Session_Node(uid: UId, name: String, dist: Boolean)

  case class Theory_Node(uid: UId, name: String)

  case class Pos_Node(uid: UId, label: String, pos: Int)

  case class Entity_Node(uid: UId, name: String, kind: Entity_Kind.Value)

  case class Edge(from: UId, to: UId, label: Edge_Label.Value)
}
