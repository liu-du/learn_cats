package sandbox

import cats.syntax.foldable._
import cats.Applicative
import cats.instances.future._
import cats.syntax.applicative._
import cats.syntax.apply._

import PrintableInstances._
import PrintableSyntax._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.Semigroupal

object CatsTraverse {
  def main(args: Array[String]): Unit = {
    val hostnames = List(
      "alpha.example.com",
      "beta.example.com",
      "gamma.demo.com",
    )

    def getUptime(hostname: String): Future[Int] =
      Future {
        Thread.sleep(2000)
        hostname.length * 60
      }
    
    val allUptimes = hostnames
      .foldLeft(Future(List.empty[Int])) { (acc, host) =>
        val uptime = getUptime(host)
        for {
          acc <- acc
          uptime <- uptime
        } yield acc :+ uptime
      }

    Await
      .result(allUptimes, 10.second)
      .print

    val allUptimes2 = Future.traverse(hostnames)(getUptime)

    Await
      .result(allUptimes2, 10.second)
      .print
    
    List.empty[Int].pure[Future]

    // Semigroupal

    
  }
}