package casestudy

import cats.kernel.Monoid
import cats.Monoid.combine
import cats.syntax.traverse
import cats.instances.int._
import cats.instances.vector._
import cats.instances.future._ // for Applicative
import cats.instances.list._ // for Traverse
import cats.syntax.traverse._ // for traverse
// import cats.syntax.functor._ // for map
// import cats.Applicative
import cats.syntax.foldable._

import sandbox.PrintableInstances._
import sandbox.PrintableSyntax._
import scala.concurrent.Await
import scala.concurrent.duration._

object MapReduce {
  def foldMap[A, B: Monoid](as: Vector[A])(f: A => B): B
    = as.map(f).foldLeft(Monoid[B].empty)(combine)

  def main(args: Array[String]): Unit = {
    import cats.instances.int._
    foldMap(Vector(1,3,4))(identity).toString().print

    import cats.instances.string._
    foldMap(Vector(1,3,4))(_.toString + "!").toString().print
    foldMap("hello world".toVector)(_.toUpper.toString).toString().print

    // parallelzing
    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits.global

    (1 to 10).toList.grouped(3).toList.toString().print

    def parallelFoldMap[A, B : Monoid](values: Vector[A])(func: A => B): Future[B] = {
      val n = Runtime.getRuntime.availableProcessors
      val groupSize = (values.length / n).ceil.toInt

      Future
        .sequence(
          values
            .grouped(groupSize)
            .map(subvec => Future { foldMap(subvec)(func) })
        )
        .map(x => x.foldLeft(Monoid[B].empty)(combine))
    }

    def catParallelFoldMap[A, B : Monoid](values: Vector[A])(func: A => B): Future[B] = {
      val n = Runtime.getRuntime.availableProcessors
      val groupSize = (values.length / n).ceil.toInt

      values
        .grouped(groupSize)
        .toVector
        .traverse(subvect => Future(foldMap(subvect)(func)))
        .map(_.combineAll)
    }

    Await.result(
      parallelFoldMap((1 to 1000).toVector)(_ * 10),
      10.seconds
    ).print

    Await.result(
      catParallelFoldMap((1 to 1000).toVector)(_ * 10),
      10.seconds
    ).print
  }
}