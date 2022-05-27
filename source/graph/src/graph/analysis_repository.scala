/* Repository layer. For performance reasons, repository must support computational methods.
 */
package isabelle.graph


import isabelle.graph.Analysis.{Frequency, Graph, Score, Scoring}
import isabelle.graph.Filter.{Edge_Props, Linear_Node, Node}


trait Analysis_Repository
{
  def count_nodes(node: Node): Long

  def count_edges(node: Node, edge: Edge_Props): Long

  def score(node: Node, to_edge: Linear_Node, edge: Edge_Props, metric: Score): Scoring

  def score_frequency(node: Node, to_edge: Linear_Node, edge: Edge_Props, metric: Score): Seq[Frequency]

  def subgraph(nodes: Filter.Node, to_edge: Filter.Linear_Node, edges: Filter.Edge_Props): Graph

  def fractal_dimension(name: String, node: Node): List[(Int, Int)]

  def average_path_length(node: Node): Double
}
