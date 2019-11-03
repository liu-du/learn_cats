package sandbox

import cats.Monad
import cats.syntax.applicative._
import cats.syntax.flatMap._
import scala.language.higherKinds

import cats.data.OptionT
import cats.instances.list._
import cats.instances.either._
import cats.instances.future._
import cats.instances.option._
import cats.data.EitherT
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Failure
import scala.util.Success
import cats.Id

object MonadTransformer {
  // def compose[M1[_]: Monad, M2[_]: Monad] = {
  //   type Composed[A] = M1[M2[A]]


  //   new Monad[Composed] {
  //     def pure[A](a: A): Composed[A] = a.pure[M2].pure[M1]
  //     def flatMap[A, B](fa: M1[M2[A]])(f: A => M1[M2[B]]): M1[M2[B]] = {
  //       fa.flatMap[M2[B]](m2a => m2a.flatMap(f))
        
  //     }
  //     def tailRecM[A, B](a: A)(f: A => Composed[Either[A,B]]): Composed[B] = ???
  //   }
  // }

  // import Printable

  import PrintableInstances._
  import PrintableSyntax._
  implicit def printableListOption[A] = new Printable[ListOption[A]] {
    def format(a: ListOption[A]): String = a.value.toString()
  }
  implicit def printableErrorOrOption[A] = new Printable[ErrorOrOption[A]] {
    def format(a: ErrorOrOption[A]): String = a.value.toString()
  }
  implicit def printableEitherOption[A] = new Printable[FutureEitherOption[A]] {
    def format(a: FutureEitherOption[A]): String = a.value.toString()
  }
  // implicit def printableEitherOption[A] = new Printable[FutureEitherOption[A]] {
  //   def format(a: FutureEitherOption[A]): String = a.value.toString()
  // }
  implicit class FutureEitherOptionOps[A](f: FutureEitherOption[A]) {
    def await = Await.result(f.value.value, 10.second)
  }
  
  
  def compose[M1[_]: Monad] = {
    type Composed[A] = M1[Option[A]]

    new Monad[Composed] {
      def pure[A](a: A): Composed[A] = Option(a).pure[M1]
      def flatMap[A, B](fa: M1[Option[A]])(f: A => M1[Option[B]]): M1[Option[B]] = {
        fa.flatMap(x => x.fold[M1[Option[B]]](Option.empty.pure[M1])(f))
      }
      def tailRecM[A, B](a: A)(f: A => Composed[Either[A,B]]): Composed[B] = ???
    }
  }

  type ListOption[A] = OptionT[List, A]

  type ErrorOr[A] = Either[String, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]


  type FutureEither[A] = EitherT[Future, String ,A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]

  def main(args: Array[String]): Unit = {
    32
      .pure[ListOption]
      .print
    
    32
      .pure[ListOption]
      .map(_ * 2)
      .print

    32
      .pure[ListOption]
      .flatMap(x => OptionT(List(Some(x), None)))
      .print

    3.333
      .pure[ErrorOrOption]
      .map(_ * 2)
      .print

    9
      .pure[FutureEitherOption]
      .await
      .toString()
      .print

    val res1 = for {
      a <- 9.pure[FutureEitherOption]
      _ = println(a)
      b <- OptionT[FutureEither, Int](EitherT[Future, String, Option[Int]]( Future { Left[String, Option[Int]]("yes") } ))
      _ = println(b)
    } yield a + b
    println(res1.await)

    val res2 = for {
      a <- 9.pure[FutureEitherOption]
      _ = println(a)
      b <- OptionT[FutureEither, Int](EitherT[Future, String, Option[Int]]( Future { Right[String, Option[Int]](None) } ))
      _ = println(b)
    } yield a + b
    println(res2.await)


    println("------- Exercise -------")

    type Response[A] = EitherT[Future, String, A]
    val powerLevels = Map(
      "Jazz" -> 6,
      "Bumblebee" -> 8,
      "Hot Rod" -> 10
    )

    def getPowerLevel(autobot: String): Response[Int] = 
      EitherT(Future{
        powerLevels.get(autobot) match {
          case Some(powerLevel) => Right(powerLevel)
          case None => Left(s"$autobot is not reachable")
        }
      })
    
    def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = 
      for {
        p1 <- getPowerLevel(ally1)
        p2 <- getPowerLevel(ally2)
      } yield p1 + p2 > 15
    
    def tacticalReport(ally1: String, ally2: String): String = 
      Await.result(
        canSpecialMove(ally1, ally2).fold(
          identity,
          if (_) s"$ally1 and $ally2 are ready to roll out!" 
          else s"$ally1 and $ally2 need a recharge"
        ),
        2.seconds
      )

    tacticalReport("Jazz", "Bumblebee").print
    tacticalReport("Bumblebee", "Hot Rod").print
    tacticalReport("Jazz", "Ironhide").print
        
  }
}