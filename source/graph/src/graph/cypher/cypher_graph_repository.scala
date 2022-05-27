/* Graph repository for cypher. Warning: not thread-safe!
 */
package isabelle.graph.cypher


import isabelle.graph.Graph.Edge_Label.{DEP, IN}
import isabelle.graph.Graph.Fields.{DISTRIBUTION, ID, KIND, LABEL, POS}
import isabelle.graph.Graph.Node_Kind.{ENTITY, POSITION, SESSION, THEORY}
import isabelle.graph.Graph._
import isabelle.graph.cypher.Cypher._
import isabelle.graph.cypher.Cypher_Filter.{Name, node_fq}
import isabelle.graph.{Filter, Graph, Graph_Repository}

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

import org.neo4j.driver.{Result, Value}


class Cypher_Graph_Repository private(data_repository: Neo4j_Data_Repository)
  extends Graph_Repository
{
  val CHUNK_SIZE: Int = 10000

  override def close(): Unit = data_repository.close()

  override def save_sessions(sessions: Seq[Session_Node]): Unit =
  {
    data_repository.run(query(
      s"""
      UNWIND $$props as props
      CREATE (s:$SESSION) SET s = props
      """,
      "props" -> p(sessions.map(session =>
        Map(
          ID.toString -> session.uid.value, LABEL.toString -> session.name, DISTRIBUTION
            .toString -> p(session.dist))
      ))
    ))
  }

  override def save_theories(theories: Seq[Theory_Node]): Unit =
  {
    data_repository.run(query(
      s"""
      UNWIND $$props as props
      CREATE (t:$THEORY) SET t = props
      """,
      "props" -> p(theories
        .map(node => Map(ID.toString -> node.uid.value, LABEL.toString -> node.name)))
    ))
  }

  override def save_positions(theory: UId, nodes: Map[Pos_Node, Seq[Entity_Node]]): Unit =
  {
    data_repository.run(query(
      s"""
     UNWIND $$props as props
     MATCH (t:$THEORY) WHERE t.$ID = $$t_id
       CREATE (p:$POSITION {
          $ID:props.$ID,
          $LABEL:props.$LABEL,
          $POS:props.$POS
        })-[:$IN]->(t)
        WITH p, props.entities AS entities
        UNWIND entities as entity_props
        CREATE (e:$ENTITY)-[:$IN]->(p)
        SET e = entity_props
    """,
      "t_id" -> p(theory.value),
      "props" -> p(nodes.map { case (node, entities) =>
        Map(
          ID.toString -> p(node.uid.value),
          LABEL.toString -> p(node.label),
          POS.toString -> p(node.pos),
          "entities" -> p(entities.map(entity =>
            Map(
              ID.toString -> entity.uid.value,
              KIND.toString -> entity.kind.toString,
              LABEL.toString -> entity.name
            )
          ))
        )
      }.toSeq)
    ))
  }

  override def save_edges(from: Node_Kind.Value, to: Node_Kind.Value, edges: Seq[Graph.Edge]): Unit =
  {
    @tailrec
    def by_label(label: Edge_Label.Value, from: Node_Kind.Value, to: Node_Kind.Value, edges: Seq[Edge]): Unit =
    {
      val (chunk, next) = edges.splitAt(CHUNK_SIZE)
      data_repository.run(query(
        s"""
      UNWIND $$props AS props
        MATCH (s:$from), (t:$to)
        WHERE s.$ID=props.src AND t.$ID=props.target
        MERGE (s)-[:$label]->(t)
      """,
        "props" -> p(chunk.map(edge =>
          Map(
            "src" -> edge.from.value,
            "target" -> edge.to.value,
            "type" -> edge.label.toString
          )
        ))
      ))
      if (next.nonEmpty) {
        commit()
        by_label(label, from, to, next)
      }
    }

    edges.groupBy(_.label).foreach {
      case (label, edges) => by_label(label, from, to, edges)
    }
  }

  private def map_sessions(res: Seq[Result]): Seq[Session_Node] =
  {
    res.flatMap(_.list().asScala)
      .map(_.get("s"))
      .map { s =>
        Session_Node(
          UId(s.get(ID.toString).asString()),
          s.get(LABEL.toString).asString(),
          s.get(DISTRIBUTION.toString).asBoolean()
        )
      }
  }

  private def get_nodes(filter: Filter.Node): Seq[Value] =
  {
    val name = Name("n")
    val fq = node_fq(name, filter)
    val fqs = fq.queries.mkString(",")
    val clauses = fq.clauses.mkString(" AND ")
    data_repository.run(query(
      s"""MATCH $fqs WHERE $clauses RETURN $name""",
      fq.params
    )).flatMap(_.list().asScala).map(_.get(name.toString))
  }

  override def get_entities(filter: Filter.Node): Seq[Entity_Node] = get_nodes(filter).map { r =>
    Entity_Node(
      UId(r.get(ID.toString).asString()), r.get(LABEL.toString).asString(), Entity_Kind
        .from_string(r.get(KIND.toString).asString()))
  }

  override def get_positions(filter: Filter.Node): Seq[Pos_Node] = get_nodes(filter).map { r =>
    Pos_Node(
      UId(r.get(ID.toString).asString()), r.get(LABEL.toString).asString(), r.get(POS.toString)
        .asInt())
  }

  override def get_theories(filter: Filter.Node): Seq[Theory_Node] = get_nodes(filter).map { r =>
    Theory_Node(UId(r.get(ID.toString).asString()), r.get(LABEL.toString).asString())
  }

  override def get_sessions(filter: Filter.Node): Seq[Session_Node] = get_nodes(filter).map { r =>
    Session_Node(
      UId(r.get(ID.toString).asString()),
      r.get(LABEL.toString).asString(),
      r.get(DISTRIBUTION.toString).asBoolean()
    )
  }

  override def list_sessions: Seq[Session_Node] =
  {
    map_sessions(data_repository.run(query(s"MATCH (s:$SESSION) RETURN s")))
  }

  override def list_sessions(dist: Boolean): Seq[Session_Node] =
  {
    map_sessions(data_repository
      .run(query(s"MATCH (s:$SESSION) WHERE s.$DISTRIBUTION=$dist RETURN s")))
  }

  override def list_theories(sessions: Seq[UId]): Seq[Theory_Node] =
  {
    data_repository.run(query(
      s"""
    UNWIND $$props as props
      MATCH (t:$THEORY)-[:$IN]->(s:$SESSION)
      WHERE s.$ID=props.$ID
      RETURN t
      """,
      "props" -> p(sessions.map(session => Map(ID.toString -> session.value)))
    ))
      .flatMap(_.list().asScala)
      .map(_.get("t"))
      .map { r =>
        Theory_Node(
          UId(r.get(ID.toString).asString()),
          r.get(LABEL.toString).asString()
        )
      }
  }

  override def session_deps: Seq[Edge] =
  {
    data_repository.run(query(
      s"""
        MATCH (s1:$SESSION)<-[:$IN]-(t1:$THEORY)-->(t2:$THEORY)-[:$IN]->(s2:$SESSION)
        RETURN DISTINCT s1.id AS from,s2.id AS to
        """
    ))
      .flatMap(_.list().asScala)
      .map(r => Edge(UId(r.get("from").asString()), UId(r.get("to").asString()), DEP))
  }

  override def delete_theories(theories: Seq[UId]): Unit =
  {
    data_repository.run(
      query(
        s"""
    UNWIND $$props as props
      MATCH (e:$ENTITY)-[:$IN]-(p:$POSITION)-[:$IN]-(t:$THEORY)
      WHERE t.$ID=props.$ID
      DETACH DELETE e, p, t
    """,
        "props" -> p(theories.map(theory => Map(ID.toString -> theory.value)))
      ),
      query(
        s"UNWIND $$props as props MATCH (t:$THEORY) WHERE t.$ID=props.$ID DETACH DELETE t",
        "props" -> p(theories.map(theory => Map(ID.toString -> theory.value)))
      )
    )
    data_repository.commit()
  }

  override def delete_sessions(sessions: Seq[UId]): Unit =
  {
    data_repository.run(
      query(s"""
        CALL apoc.periodic.iterate('
          MATCH (e:$ENTITY)--(p:$POSITION)--(t:$THEORY)--(s:$SESSION)
          WHERE s.$ID IN $$ids
          RETURN e',
        'DETACH DELETE e', {batchSize:10000, iterateList:true, params:{ids: $$s_ids}})""",
        "s_ids" -> p(sessions.map(_.value))),
      query(s"""
        CALL apoc.periodic.iterate('
          MATCH (p:$POSITION)--(t:$THEORY)--(s:$SESSION)
          WHERE s.$ID IN $$ids
          RETURN p',
        'DETACH DELETE p', {batchSize:10000, iterateList:true, params:{ids: $$s_ids}})""",
        "s_ids" -> p(sessions.map(_.value))),
      query(s"""
        UNWIND $$props as props
        MATCH (t:$THEORY)-[:$IN]-(s:$SESSION)
        WHERE s.$ID=props.$ID
        DETACH DELETE t, s""",
        "props" -> p(sessions.map(session => Map(ID.toString -> session.value)))))
    data_repository.commit()
  }

  override def commit(): Unit = data_repository.commit()
}

object Cypher_Graph_Repository
{
  def apply(data_repository: Neo4j_Data_Repository): Cypher_Graph_Repository =
  {
    data_repository.commit()
    data_repository.run(
      query(s"CREATE INDEX IF NOT EXISTS FOR (s:$SESSION) ON (s.$ID)"),
      query(s"CREATE INDEX IF NOT EXISTS FOR (s:$SESSION) ON (s.$LABEL)"),
      query(s"CREATE INDEX IF NOT EXISTS FOR (s:$SESSION) ON (s.$DISTRIBUTION)"),
      query(s"CREATE INDEX IF NOT EXISTS FOR (t:$THEORY) ON (t.$ID)"),
      query(s"CREATE INDEX IF NOT EXISTS FOR (t:$THEORY) ON (t.$LABEL)"),
      query(s"CREATE INDEX IF NOT EXISTS FOR (p:$POSITION) on (p.$LABEL)"),
      query(s"CREATE INDEX IF NOT EXISTS FOR (p:$POSITION) on (p.$POS)"),
      query(s"CREATE INDEX IF NOT EXISTS FOR (e:$ENTITY) on (e.$ID)"),
      query(s"CREATE INDEX IF NOT EXISTS FOR (e:$ENTITY) on (e.$LABEL)"),
      query(s"CREATE INDEX IF NOT EXISTS FOR (e:$ENTITY) on (e.$POS)"),
      query(s"CREATE INDEX IF NOT EXISTS FOR (e:$ENTITY) on (e.$KIND)")
    )
    data_repository.commit()

    new Cypher_Graph_Repository(data_repository)
  }
}
