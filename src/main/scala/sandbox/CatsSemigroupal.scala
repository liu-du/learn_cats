package sandbox

import cats.Semigroupal
import cats.data.Validated
import cats.instances.list._
import cats.instances.option._
import cats.instances.future._
import cats.syntax.applicativeError._
import cats.syntax.apply._
import cats.syntax.validated._
import cats.data.NonEmptyVector
import cats.syntax.either._


import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._

object CatsSemigroupal {
  import PrintableInstances._
  import PrintableSyntax._
  implicit def printableSemigroupal[A[_]] = new Printable[Semigroupal[A]] {
    def format(a: Semigroupal[A]): String = a.toString()
  }

  def main(args: Array[String]): Unit = {
    Semigroupal[Option].product(Some(123), Some("abc")).print


    (Option(1), Option("abc"), Option(3.3)).tupled.print

    case class Box(a: Int, b: String, c: Double)

    println((Option(1), Option("abc"), Option(3.3)).mapN(Box))

    // Semigroupal[Future] actually ** runs in parallel **
    // val a = Semigroupal[Future].product(Future {Thread.sleep(10000); 123}, Future {Thread.sleep(10000); "abc"})
    // Await.result(a, 20.seconds).print


    type AllErrorsOr[A] = Validated[List[String], A]
    Semigroupal[AllErrorsOr].product(
      Validated.invalid(List("invalid 1")),
      Validated.invalid(List("invalid 2"))
    )
    .toString
    .print

    List("bad").raiseError[AllErrorsOr, Int]
      .toString()
      .print

    (List(404).invalid[Int], List(500).invalid[Int]).tupled.toString().print
    (NonEmptyVector.of("Error 1"), NonEmptyVector.of("Error 2")).tupled.toString().print


    (List(404).invalid[Int], List(500).invalid[Int]).tupled.toEither.toString().print

    300.valid[List[String]].ensure(List("status code is 2xx"))(x => x >= 200 && x < 300).toString().print


    case class User(name: String, age: Int)

    def readName(m: Map[String, String]): Validated[List[String], String] = 
      m.get("name") match {
        case None => Validated.invalid(List("no name field!"))
        case Some(value) => Validated.valid(value)
      }
    
    def readAge(m: Map[String, String]): Validated[List[String], Int] = 
      Validated
        .fromOption(m.get("age"), List("no age field!"))
        .andThen(x => x.toIntOption match {
          case None => Validated.invalid(List("non integer age"))
          case Some(value) if value < 0 => Validated.invalid(List("age < 0"))
          case Some(value) => Validated.valid(value)
        })

    def readUser(m: Map[String, String]): Validated[List[String], User] = 
        (readName(m), readAge(m)).mapN(User)
      
    readUser(Map("name" -> "fdsa", "age" -> "1")).toString().print

  }
}