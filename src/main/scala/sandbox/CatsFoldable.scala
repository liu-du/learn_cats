package sandbox

import PrintableInstances._
import PrintableSyntax._

import cats.Foldable
import cats.syntax.foldable._
import cats.instances.list._
import cats.instances.option._

object CatsFoldable {

  def map[A, B](l: List[A])(f: A => B) = {
    l.foldRight(List.empty[B])((a, b) => f(a) :: b)
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]) = {
    l.foldRight(List.empty[B])((a, b) => f(a) ++ b)
  }

  def filter[A](l: List[A])(f: A => Boolean) = {
    l.foldRight(List.empty[A])((a, acc) => if (f(a)) a:: acc else acc)
  }

  def sum(l: List[Int]) = 
    l.foldRight(0)(_ + _)

  def main(args: Array[String]): Unit = {
    map(List(1,2,3))(_ * 2)
      .toString
      .print

    flatMap(List(1,2,3))(x => List(x, x * 2))
      .toString
      .print

    filter(List(1,2,3))(_ >= 2)
      .toString
      .print
    
    sum(List(1,2,3))
      .print



    println("------- Cats Foldable -------")

    List(1,2,3)
      .foldl(0)(_ + _)
      .print

    import cats.instances.int._
    List(1,2,3)
      .combineAll
      .print

    import cats.instances.string._
    List("a", "b", "c")
      .combineAll
      .print

    List(1,2,3)
      .foldMap(_.toString)
      .print

    List(List(1,2,3), List(4,5,6))
      .combineAll
      .print
    
    (Foldable[List] compose Foldable[List])
      .combineAll(
        List(List(1,2,3), List(4,5,6))
      )
      .print

    val maybeInt = Option(2)
    Foldable[Option]
      .foldLeft(maybeInt, 10)(_ * _)
      .print

  }
}