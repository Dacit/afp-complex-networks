/* Core entities
 */
package isabelle.graph


import isabelle.graph.Filter.Dir
import isabelle.graph.Graph.{Node_Kind, UId}


object Analysis
{
  sealed trait Score {}

  object Score
  {
    /* Sum of inverse distance to all other nodes.
       Harmonic: sum outside; unconnected -> 0 */

    case class Closeness_Centrality(harmonic: Boolean = true) extends Score

    /* Page rank in graph */

    case object Eigenvector_Centrality extends Score


    /* Relative number of degrees */

    case class Degree_Centrality(dir: Dir) extends Score
    {
      require(dir != Dir.Both)
    }

    /* Score for number of shortest paths going through node */

    case object Betweenness_Centrality extends Score


    /* Probability that neighbours are connected (undirected) */

    case object Clustering_Coefficient extends Score


    /* Local connectedness of nodes vs neighbours */

    case object Effective_Size extends Score

    /* Number of nodes */

    case class Count(node_kind: Node_Kind.Value) extends Score
  }

  case class Bin(start: Double, size: Double, count: Long)

  class Distribution private(val sorted: IndexedSeq[Frequency])
  {
    private def get_start(start: Option[Double]): Double = start
      .orElse(sorted.headOption.map(_.value)).getOrElse(0)

    private def get_end(end: Option[Double]): Double = end.orElse(sorted.lastOption.map(_.value))
      .getOrElse(0)

    private def bins(num: Int, size: Double, start: Double) =
    {
      var elems = sorted
      Range(0, num).map { i =>
        val bin = sorted.takeWhile(_.value < start + (i + 1) * size)
        elems = elems.drop(bin.length)
        Bin(start + i * size, size, elems.map(_.count).sum)
      }
    }

    def nbins(num: Int, start: Option[Double] = None, end: Option[Double] = None): Seq[Bin] =
    {
      if (num < 1) throw new IllegalArgumentException(s"Invalid number of bins: ${ num }")
      val start_ = get_start(start)
      val bin_size = Math.ceil((get_end(end) - start_) / num.toDouble)
      bins(num, bin_size, start_)
    }

    def bins(size: Double, start: Option[Double] = None): Seq[Bin] =
    {
      if (size <= 0) throw new IllegalArgumentException(s"Invalid bin size: ${ size }")
      val start_ = get_start(start)
      val end: Double = sorted.lastOption.map(_.value).getOrElse(0)
      val num = Math.ceil((end - start_) / size).toInt
      bins(num, size, start_)
    }
  }

  object Distribution
  {
    def apply(frequencies: Seq[Frequency]): Distribution = new Distribution(frequencies
      .sortBy(_.value).toVector)
  }

  /* Statistics */

  case class Stats(nodes: Long, edges: Long)


  /* Component */

  case class Component(elements: Seq[UId])


  /* Scoring */

  case class Scoring(node_scores: Map[UId, Double])


  /* Distribution */

  case class Frequency(value: Double, count: Long)


  /* Aggregations */

  case class Graph(nodes: Seq[UId], edges: Seq[(UId, Long, UId)])
}
