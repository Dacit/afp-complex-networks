package isabelle.graph.cypher


import java.util.concurrent.TimeUnit

import org.neo4j.driver._
import org.neo4j.driver.exceptions.ClientException


class Neo4j_Data_Repository private(neo4j: Session) extends AutoCloseable
{
  private var _tx: Option[Transaction] = None

  def commit(): Unit = _tx.foreach { tx =>
    tx.commit()
    tx.close()
    _tx = None
  }

  private def run_single(query: Query, tx: Transaction): Result = try {
    tx.run(query)
  } catch {
    case e: ClientException => throw new IllegalArgumentException(
      "Error during query:\n\n" + query.text() + "\nCause: " + e.toString)
  }

  def run(queries: Query*): Seq[Result] =
  {
    val tx = _tx.getOrElse { _tx = Some(neo4j.beginTransaction()); _tx.get }
    queries.map(run_single(_, tx))
  }

  override def close(): Unit =
  {
    _tx.foreach(_.close())
    neo4j.close()
  }
}

object Neo4j_Data_Repository
{
  val PROTOCOL = "bolt"
  val SYS_DB = "system"
  val DEFAULT_DB = "neo4j"

  def remote(address: String, auth: AuthToken = AuthTokens.none()): Neo4j_Data_Repository =
  {
    val config = Config.builder.withConnectionTimeout(1, TimeUnit.MINUTES)
      .withConnectionAcquisitionTimeout(1, TimeUnit.MINUTES)
      .withConnectionLivenessCheckTimeout(1, TimeUnit.MINUTES)
      .build
    val driver = GraphDatabase.driver(PROTOCOL + "://" + address, auth, config)
    new Neo4j_Data_Repository(driver.session()) {
      override def close(): Unit =
      {
        super.close()
        driver.close()
      }
    }
  }
}
