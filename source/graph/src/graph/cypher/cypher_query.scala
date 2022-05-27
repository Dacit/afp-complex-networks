package isabelle.graph.cypher


import isabelle.graph.Filter.Value
import isabelle.graph.Filter.Value.{Boolean_Value, Double_Value, Int_Value, String_Value}

import scala.jdk.CollectionConverters._

import org.neo4j.driver.Query


trait Cypher_Param[A]
{
  def to_object(a: A): Object
}

object Cypher
{
  type Params = Map[String, Seq[Value]]

  implicit val obj_param: Cypher_Param[Object] = (o: Object) => o

  implicit val string_param: Cypher_Param[String] = (s: String) => s.asInstanceOf[Object]

  implicit val int_param: Cypher_Param[Int] = (i: Int) => i.asInstanceOf[Object]

  implicit val bool_param: Cypher_Param[Boolean] = (o: Boolean) => o.asInstanceOf[Object]

  implicit def seq_param[V](implicit ev: Cypher_Param[V]): Cypher_Param[Seq[V]] = (s: Seq[V]) => s
    .map(ev.to_object).asJava

  implicit def map_param[V](implicit ev: Cypher_Param[V]): Cypher_Param[Map[String, V]] =
    (m: Map[String, V]) => m.view.mapValues(ev.to_object).toMap.asJava
      .asInstanceOf[Object]

  def p[A](param: A)(implicit ev: Cypher_Param[A]): Object = ev.to_object(param)

  def query[A](text: String, params: (String, Object)*): Query = new Query(
    text, params.toMap.asJava)

  def cypher_value(value: Value): Object = value match {
    case String_Value(inner) => inner
    case Int_Value(inner) => inner.asInstanceOf[Object]
    case Double_Value(inner) => inner.asInstanceOf[Object]
    case Boolean_Value(inner) => inner.asInstanceOf[Object]
  }

  def query[A](text: String, params: Params): Query =
  {
    val p = params.view.mapValues(v => v.map(cypher_value).asJava.asInstanceOf[Object]).toMap.asJava
    new Query(text, p)
  }
}
