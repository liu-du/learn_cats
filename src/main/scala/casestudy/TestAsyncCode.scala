package casestudy

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


import cats.Id
import cats.instances.future._ // for Applicative
import cats.instances.list._ // for Traverse
import cats.syntax.traverse._ // for traverse
import cats.syntax.functor._ // for map
import cats.Applicative

// abstract over F
trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

// real client would be extending UptimeClient[Future]
trait RealUptimeClient extends UptimeClient[Future]

// test client extends uptimeClient[Id]
class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
  def getUptime(hostname: String): Id[Int] = hosts.getOrElse(hostname, 0)
}

// traverse requires an instance of Applicative in implicit scope
class UptimeService[F[_] : Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int]
    = hostnames.traverse(client.getUptime).map(_.sum)
}

object TestAsyncCode {
  def main(args: Array[String]): Unit = {

    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts) // instantiate a synchronous test client
    val service = new UptimeService(client) // inject test client

    assert( service.getTotalUptime(List("host1", "host2")) == hosts.values.sum )
  }
}