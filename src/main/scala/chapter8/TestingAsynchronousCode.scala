package chapter8

object TestingAsynchronousCode {

  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global

  import cats.instances.future._
  import cats.instances.list._

  import cats.syntax.traverse._
  import cats.syntax.functor._

  import cats.Id
  import cats.Applicative

  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  trait RealUptimeClient extends UptimeClient[Future] {
    def getUptime(hostname: String): Future[Int]
  }

  //trait TestUptimeClient extends UptimeClient[Id] {
  //  def getUptime(hostname: String): Int
  //}

  class UptimeService[F[_] : Applicative](client: UptimeClient[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =
      hostnames.traverse(client.getUptime).map(_.sum)
  }

  class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
    override def getUptime(hostname: String): Id[Int] =
      hosts.getOrElse(hostname, 0)
  }

  def testTotalUptime(): Unit = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

  def main(args: Array[String]): Unit = {
    testTotalUptime()
  }
}
