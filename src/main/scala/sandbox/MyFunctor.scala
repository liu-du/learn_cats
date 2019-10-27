package sandbox

import sandbox.MonoidAndSemigroup.Monoid
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.global
import scala.concurrent.Await
import scala.concurrent.duration._

object MyFunctor {

  trait Functor[F[_]] {
    def lift[T, U](func: T => U): F[T] => F[U] = fmap(_)(func)
    def fmap[T, U](functor: F[T])(func: T => U): F[U]
  }

  // interface object
  object Functor {
    // summoner
    def apply[F[_]](implicit f: Functor[F]) = f

    // implicit instance
    implicit val listFunctor = new Functor[List] {
      def fmap[T, U](l: List[T])(func: T => U): List[U] = l.map(func)
    }

    // pass in other implicit instances via implicit params (depedency injection via implicit)
    implicit def futureFunctor(implicit ec: ExecutionContext) = 
      new Functor[Future] {
        def fmap[T, U](functor: Future[T])(func: T => U): Future[U] = functor.map(func)
      }
  }

  // interface syntax
  implicit class FunctorOps[F[_], T](fa: F[T]) {
    def fmap[U](func: T => U)(implicit functor: Functor[F]): F[U] = 
      functor.fmap(fa)(func)
  }

  def main(args: Array[String]): Unit = {

    // summon implicit
    println(Functor.apply[List].fmap(List(1,2,3))(_ * 2))
    println(implicitly[Functor[List]].fmap(List(1,2,3))(_ * 2))

    // lift function into Functor context and apply
    println(Functor.apply[List].lift[Int, Int](_ * 2)(List(1,2,3)))

    // BEST: interface syntax (via implicit class conversion)
    println(List(1,2,3).fmap(_ * 2))

    implicit val ec = global
    val future = Future {
      Thread.sleep(1000L)
      "I'm a future"
    }

    Await.result(
      future
        .fmap(_ + "!")
        .fmap(println), 
      2.seconds
    )

  }

}