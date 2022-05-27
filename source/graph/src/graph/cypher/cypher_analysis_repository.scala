package isabelle.graph.cypher


import isabelle.{Time, error}
import isabelle.graph.Analysis.Score.{Clustering_Coefficient, Degree_Centrality}
import isabelle.graph.Analysis.{Frequency, Graph, Score, Scoring}
import isabelle.graph.Filter.{Dir, Edge_Props, Linear_Node, Node}
import isabelle.graph.Graph.Fields.{ID, KIND}
import isabelle.graph.Graph.{Node_Kind, UId}
import isabelle.graph.cypher.Cypher._
import isabelle.graph.cypher.Cypher_Filter.{Name, edge_props_query, linear_node_query, node_fq}
import isabelle.graph.{Analysis_Repository, Filter}

import scala.jdk.CollectionConverters._


class Cypher_Analysis_Repository(data_repo: Neo4j_Data_Repository) extends Analysis_Repository
{
  /* Query keywords to avoid typos */

  private val MATCH = "MATCH"
  private val WHERE = "WHERE"
  private val _AND_ = " AND "
  private val RETURN = "RETURN"
  private val WITH = "WITH"
  private val AS = "AS"
  private val COUNT = "COUNT"
  private val CALL = "CALL"
  private val YIELD = "YIELD"
  private val NODE_ID = "nodeId"
  private val SCORE = "score"
  private val DISTINCT = "distinct"
  private val FREQ = "freq"

  override def count_nodes(node: Node): Long =
  {
    val n = Name("n")
    val fq = node_fq(n, node)
    val NODES = "nodes"
    data_repo.run(query(
      s"""
      $MATCH ${ fq.queries.mkString(",") }
      $WHERE ${ fq.clauses.mkString(_AND_) }
      $RETURN $COUNT($n) $AS $NODES
      """,
      fq.params
    )).map(_.next().get(NODES).asLong()).head
  }

  override def count_edges(node: Node, edge: Edge_Props): Long =
  {
    val l_name = Name("l")
    val e_name = Name("e")
    val r_name = Name("r")
    val l = node_fq(l_name, node)
    val e = edge_props_query(Some(e_name), edge)
    val r = node_fq(r_name, node)
    val EDGES = "edges"
    data_repo.run(query(
      s"""
      $MATCH ${ l.queries.mkString(",") }
      $WHERE ${ l.clauses.mkString(_AND_) }
      $WITH $l_name
      $MATCH ${ r.queries.mkString(",") },($l_name)$e($r_name)
      $WHERE ${ r.clauses.mkString(_AND_) }
      $RETURN $COUNT($e_name) $AS $EDGES
      """,
      l.params ++ r.params
    )).map(_.next().get(EDGES).asLong()).head
  }

  private def check_nodes(nodes: Filter.Node): (String, Params) =
  {
    val n = Name("n")
    val n_fq = node_fq(n, nodes)
    val row_q =
      s"""
     $CALL {
       $MATCH ${ n_fq.queries.mkString(",") }
       $WHERE ${ n_fq.clauses.mkString(_AND_) }
       $RETURN $DISTINCT {} $AS exists
     } $WITH exists """
    (row_q, n_fq.params)
  }

  private def projection(nodes: Filter.Node, to_edge: Linear_Node, edges: Edge_Props,
    count: Boolean = false): (String, String, String, Params) =
  {
    val n = Name("n")
    val n_fq = node_fq(n, nodes)
    val ln = Name("ln")
    val ln_fq = linear_node_query(ln, to_edge)

    val queries = ln_fq match {
      case Some(fq) => (fq.query + n_fq.queries.head) +: n_fq.queries.tail
      case None => n_fq.queries
    }
    val clauses: Iterable[String] = ln_fq.map(_.clauses).getOrElse(Nil) ++ n_fq.clauses
    val params = n_fq.params ++ ln_fq.map(_.params).getOrElse(Map.empty)

    val r = Name("r")
    val e_q = edge_props_query(Some(r), edges)

    val node_query = s"""'
        $MATCH ${ n_fq.queries.mkString(",") }
        $WHERE ${ n_fq.clauses.mkString(_AND_) }
        $RETURN id($n) $AS id'"""
    val relationship_query = s"""'
        $MATCH ${ queries.mkString(",") }
        $WHERE ${ clauses.mkString(_AND_) }
        $WITH $n $AS src ${ if (ln_fq.isDefined) s", $ln $AS src_e" else "" }
        $MATCH (${ if (ln_fq.isDefined) "src_e" else "src" })$e_q${ queries.mkString(",") }
        $WHERE ${ clauses.mkString(_AND_) }
        $RETURN id(src) $AS source, id($n) $AS target${ if (count) s" , count($r) as $COUNT" else "" }'"""
    val parameters = s"""{ ${ params.keys.map(p => s"$p:$$$p").mkString(",") } }"""
    (node_query, relationship_query, parameters, params)
  }

  private def node_projection(nodes: Filter.Node, to_edge: Linear_Node): (String, Params) =
  {
    val n_name = Name("n")
    val n_fq = node_fq(n_name, nodes)
    val ln_name = Name("ln")
    val ln_fq = linear_node_query(ln_name, to_edge)

    val queries = ln_fq match {
      case Some(fq) => (fq.query + n_fq.queries.head) +: n_fq.queries.tail
      case None => n_fq.queries
    }
    val clauses: Iterable[String] = ln_fq.map(_.clauses).getOrElse(Nil) ++ n_fq.clauses
    val params = n_fq.params ++ ln_fq.map(_.params).getOrElse(Map.empty)

    val projection =
      s"""
      $MATCH ${ queries.mkString(",") }
      $WHERE ${ clauses.mkString(_AND_) }
      $WITH id($n_name) $AS $NODE_ID, count($ln_name) $AS $SCORE
      """
    (projection, params)
  }

  private def score_query(projection: String, metric: Score): String = metric match {
    case Score.Closeness_Centrality(harmonic) =>
      val method = if (harmonic) "closeness.harmonic" else "closeness"
      s"$CALL gds.alpha.$method.stream({$projection}) $YIELD $NODE_ID, centrality $AS $SCORE"
    case Score.Eigenvector_Centrality =>
      s"$CALL gds.eigenvector.stream({$projection}) $YIELD $NODE_ID, $SCORE"
    case Degree_Centrality(dir) =>
      s"$CALL gds.degree.stream({$projection, orientation: '${ orientation(dir) }'}) $YIELD $NODE_ID, $SCORE"
    case Score.Clustering_Coefficient =>
      s"$CALL gds.localClusteringCoefficient.stream({$projection}) $YIELD $NODE_ID," +
        s" localClusteringCoefficient $AS $SCORE"
    case Score.Betweenness_Centrality =>
      s"$CALL gds.betweenness.stream({$projection}) $YIELD $NODE_ID, $SCORE"
  }

  private def get_all_res(score_query: String, params: Params): Scoring =
  {
    val res = data_repo.run(query(
      s"$score_query RETURN gds.util.asNode($NODE_ID).$ID $AS $ID, $SCORE",
      params
    )).flatMap(_.list().asScala)
      .map(r => (UId(r.get(ID.toString).asString()), r.get(SCORE).asDouble()))
      .toMap
    Scoring(res)
  }

  private def get_freqs(score_query: String, params: Params): Seq[Frequency] =
  {
    data_repo.run(query(
      s"$score_query RETURN count($NODE_ID) $AS $FREQ, $SCORE",
      params
    )).flatMap(_.list().asScala)
      .map(r => Frequency(r.get(SCORE).asDouble(), r.get(FREQ).asLong()))
  }

  private def orientation(dir: Dir): String = dir match {
    case Dir.To => "NATURAL"
    case Dir.From => "REVERSE"
    case Dir.Any => "UNDIRECTED"
    case Dir.Both => throw new IllegalArgumentException("Invalid orientation")
  }

  override def score(nodes: Filter.Node, to_edge: Linear_Node, edges: Edge_Props, kind: Score): Scoring = kind match {
    case Score.Count(_) =>
      val (query, params) = node_projection(nodes, to_edge)
      get_all_res(query, params)
    case Score.Effective_Size =>
      val ccl = score(nodes, to_edge, edges, Clustering_Coefficient)
      val deg = score(nodes, to_edge, edges, Degree_Centrality(Dir.Any))
      Scoring(deg.node_scores.keys.map { i =>
        val k = deg.node_scores(i)
        val c = ccl.node_scores(i)
        i -> (k - (k - 1.0) * c)
      }.toMap)
    case _ =>
      val (node_q, rel_q, par_q, proj_params) = projection(nodes, to_edge, edges)
      val proj_q = s"nodeQuery: $node_q, relationshipQuery: $rel_q, parameters: $par_q"
      val (safe_q, safe_params) = check_nodes(nodes)
      val score_q = score_query(proj_q, kind)
      get_all_res(safe_q + score_q, safe_params ++ proj_params)
  }

  override def score_frequency(
    nodes: Filter.Node,
    to_edge: Linear_Node,
    edges: Filter.Edge_Props,
    kind: Score
  ): Seq[Frequency] =
    kind match {
      case Score.Effective_Size => ???
      case Score.Count(_) =>
        val (query, params) = node_projection(nodes, to_edge)
        get_freqs(query, params)
      case _ =>
        val (node_q, rel_q, par_q, proj_params) = projection(nodes, to_edge, edges)
        val proj_q = s"nodeQuery: $node_q, relationshipQuery: $rel_q, parameters: $par_q"
        val (safe_q, safe_params) = check_nodes(nodes)
        val score_q = score_query(proj_q, kind)
        get_freqs(safe_q + score_q, safe_params ++ proj_params)
    }

  override def subgraph(nodes: Node, to_edge: Linear_Node, edges: Edge_Props): Graph =
  {
    val g_nodes = score(nodes, to_edge, edges, Score.Count(Node_Kind.ENTITY)).node_scores.keys.toSeq

    val name = "sub" + Time.now().ms

    val (node_q, rel_q, par_q, params) = projection(nodes, to_edge, edges, count = true)

    val proj_q = s"$CALL gds.graph.create.cypher('$name', $node_q, $rel_q, { parameters: $par_q })"
    data_repo.run(query(proj_q, params))

    val g_edges = data_repo.run(query(
      s"""
    $CALL gds.graph.streamRelationshipProperty('$name', '$COUNT')
    $YIELD sourceNodeId, targetNodeId, propertyValue $AS $COUNT
    $RETURN gds.util.asNode(sourceNodeId).id, $COUNT, gds.util.asNode(targetNodeId).id
    """)).flatMap(_.list().asScala)
      .map(r => (UId(r.get(0).asString()), r.get(1).asLong(), UId(r.get(2).asString())))

    data_repo.run(query(s"$CALL gds.graph.drop('$name')"))

    Graph(g_nodes, g_edges)
  }

  override def fractal_dimension(name: String, nodes: Node): List[(Int, Int)] =
  {
    val num_nodes = count_nodes(nodes)

    if (num_nodes == 0L) return Nil
    data_repo.run(query(s"$CALL gds.graph.drop('$name', false)"))
    val (node_q, rel_q, par_q, proj_params) = projection(nodes, Linear_Node(Nil), Edge_Props(None, directed = false))
    val proj_q = s"""
      $CALL gds.graph.create.cypher('$name', $node_q, "
          $CALL gds.alpha.allShortestPaths.stream({
            nodeQuery: $node_q,
            relationshipQuery: $rel_q,
            parameters: $par_q
          })
          $YIELD sourceNodeId $AS source, targetNodeId as target, distance",
        { parameters: $par_q }) """
    data_repo.run(query(proj_q, proj_params))

    val l_max_res = data_repo.run(query(
      s"$CALL gds.graph.streamRelationshipProperty('$name', 'distance') $YIELD propertyValue $RETURN max(propertyValue) $AS l"
    )).map(_.single().get("l")).head
    if (l_max_res.isNull) {
      data_repo.run(query(s"$CALL gds.graph.drop('$name')"))
      return Nil
    }
    val l_max = l_max_res.asInt

    def N_b(l_b: Int): Option[Int] =
    {
      data_repo.run(query(s"$CALL gds.graph.drop('${name + l_b}', false)"))
      data_repo.run(query(
        s"$CALL gds.beta.graph.create.subgraph('${name + l_b}', '$name', '*', 'r.distance >= ${l_b.toDouble}')"))
      val n_res = data_repo.run(query(s"$CALL gds.beta.k1coloring.stats('${name + l_b}') YIELD colorCount RETURN colorCount")).map(_.single().get("colorCount")).head
      if (n_res.isNull) None
      else {
        data_repo.run(query(s"$CALL gds.graph.drop('${name + l_b}')"))
        Some(n_res.asInt)
      }
    }

    val res = Range.inclusive(1, l_max).flatMap(l_b => if(l_b == 1) Some(l_b -> count_nodes(nodes).toInt) else N_b(l_b).map(l_b -> _)).toList
    data_repo.run(query(s"$CALL gds.graph.drop('$name')"))
    res
  }

  override def average_path_length(nodes: Node): Double =
  {
    val num_nodes = count_nodes(nodes)

    if (num_nodes == 0L) Double.NaN
    else {
      val (node_q, rel_q, par_q, proj_params) = projection(nodes, Linear_Node(Nil), Edge_Props(None, directed = false))
      val proj_q =
        s"""
      $CALL gds.alpha.allShortestPaths.stream({
        nodeQuery: $node_q,
        relationshipQuery: $rel_q,
        parameters: $par_q
      }) $YIELD distance
      $RETURN avg(distance) $AS avg"""
      val res = data_repo.run(query(proj_q, proj_params)).map(_.single().get("avg")).head
      if (res.isNull) Double.NaN
      else res.asDouble()
    }
  }
}

object Cypher_Analysis_Repository
{
  def apply(data_repo: Neo4j_Data_Repository): Cypher_Analysis_Repository =
    new Cypher_Analysis_Repository(data_repo)
}
