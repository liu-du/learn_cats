package sandbox

import cats.data.Writer
import cats.instances.vector._
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.writer._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import PrintableInstances._
import PrintableSyntax._

object WriterMonad {


  def main(args: Array[String]): Unit = {

    val writer = Writer(List("this year"), 2019)

    val res1 = writer
      .flatMap(x => Writer(List("times 2"), x * 2))
      .flatMap(x => Writer(List("make a tuple"), (x, x)))
      .mapWritten(x => x.mkString(";"))
      
    println(res1)

    type Logged[A] = Writer[Vector[String], A]

    implicit val printableLogged = new Printable[Logged[Int]] {
      def format(a: Logged[Int]): String = a.toString
    }
    implicit def printableVector[A] = new Printable[Vector[A]] {
      def format(a: Vector[A]): String = a.toString
    }
    implicit def printableProduct[A, B] = new Printable[Tuple2[A, B]] {
      def format(a: (A, B)): String = a.toString
    }


    val res2 = 10.pure[Logged]
      .flatMap(a => Vector("no op").tell
        .flatMap(_ => 32.writer(Vector("lift 32"))
          .map(b => a + b)
        )
      )
      
    res2.run.print

    val res3 = for {
      a <- 10.pure[Logged]
      _ <- Vector("no op").tell
      b <- 32.writer(Vector("lift 32"))
    } yield a + b

    res3.run.print

    def slowly[A](body: => A) = try body finally Thread.sleep(200)
    def factorial(n: Int): Logged[Int] = {
      val ans = slowly(if (n == 1) 1.pure[Logged] else factorial(n - 1).map(_ * n))
      ans.tell(Vector(s"fact $n ${ans.value}"))
    }

    factorial(5).run.print

    Await.result(Future.sequence(Vector(
      Future(factorial(5).run),
      Future(factorial(10).run)
    )), 5.seconds)
    .print

  }
}