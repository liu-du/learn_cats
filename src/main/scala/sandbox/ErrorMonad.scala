package sandbox

import cats.MonadError
import cats.instances.either._
import cats.syntax.monadError._
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.instances.try_._
import cats.instances.option._
import cats.instances.future._

import scala.util.Try
import scala.concurrent.Future


object ErrorMonad {
  // trait MonadError[F[_], E] extends Monad[F] {
  //   // lift an error into the `F` context
  //   def raiseError[A](e: E): F[A]

  //   // Handle an error, potentially recovering from it:
  //   def handleError[A](fa: F[A])(f: E => A): F[A]

  //   // failing if the predicate is not satisfied
  //   def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]

  // }
  def main(args: Array[String]): Unit = {
    import PrintableSyntax._
    import PrintableInstances._
    implicit def printableErrorOr[A] = new Printable[ErrorOr[A]] {
      def format(a: ErrorOr[A]): String = a.toString
    }


    type ErrorOr[A] = Either[String, A]

    val monadError = MonadError[ErrorOr, String]

    val success = 42.pure[ErrorOr]
    val failure = "Bad".raiseError[ErrorOr, Int]

    failure.print

    monadError.handleErrorWith(failure) {
      case "Bad" => monadError.pure(-1)
      case _ => monadError.raiseError("it's not ok")
    }.print

    success
      .ensure("Number too small")(_ > 50)
      .print

    val exn: Throwable = new RuntimeException("something went wrong")
    val tried = exn.raiseError[Try, Int]

  }
}