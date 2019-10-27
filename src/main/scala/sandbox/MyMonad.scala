package sandbox

import cats.Id
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.either._

object MyMonad {

  trait Monad[F[_]] {
    def unit[A](value: A): F[A]
    def >>=[A, B](fa: F[A])(fun: A => F[B]): F[B]
    def fmap[A, B](fa: F[A])(fun: A => B): F[B] = >>=(fa)(fun andThen unit)
  }

  object Monad {
    // expose unit method on companion object
    def unit[F[_], A](a: A): F[A] = unit(a)

    // monad instances
    implicit def listMonad[A] = new Monad[List] {
      def unit[A](value: A): List[A] = List(value)
      def >>=[A, B](fa: List[A])(fun: A => List[B]): List[B] = fa.flatMap(fun)
    }
  }

  // extension methods on F[A]
  implicit class monadOps[F[_], A](fa: F[A]) {
    def >>=[B](fun: A => F[B])(implicit monad: Monad[F]): F[B] = monad.>>=(fa)(fun)
    def fmap[B](fun: A => B)(implicit monad: Monad[F]): F[B] = monad.fmap(fa)(fun)
  }

  // extension methods on A to lift from A to F[A]
  implicit class monadUnitOps[F[_], A](a: A) {
    def liftToList(implicit m: Monad[F]): F[A] = m.unit(a)
  }

  def main(args: Array[String]): Unit = {
    import PrintableInstances._
    import PrintableSyntax._
    implicit val printableEither = PrintableInstances.pure[Either[String, Int]](_.toString)


    2.liftToList
      .>>= (x => List(x, x * 10)) 
      .>>= (x => List(x * x)) 
      .print
    
    List(1,2,3)
      .fmap(_ * 10)
      .print
  
    // R style %>% by using Id Monad
    (10: Id[Int])
      .map(_ * 2)
      .print

    3.asRight[String]
      .map(_ * 2)
      .print

    "Error".asLeft[Int]
      .recoverWith {
        case str: String => Right(99)
      }
      .recoverWith {
        case str: String => Right(8888)
      }
      .print
  }
}