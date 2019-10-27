package sandbox

import cats.Monoid 
import cats.Semigroup
import cats.instances.string._
import cats.instances.int._
import cats.instances.option._
import cats.syntax.monoid._

object CatsMonoid {

  case class Order(totalCost: Double, quantity: Double)
  implicit val orderMonoid: Monoid[Order] = Monoid.instance[Order](
    Order(0, 0),
    { case (Order(c1, q1), Order(c2, q2)) => Order(c1 + c2, q1 + q2) }
  )

  def add[A: Monoid](items: List[A]): A = 
    items.foldLeft(Monoid[A].empty)(_ |+| _)

  def main(args: Array[String]): Unit = {

    // monoid string composition
    println(Monoid[String].combine("Hi", "monoid"))
    println(Semigroup[String].combine("Hi", "semigroup"))

    // monoid option[int] composition
    println(Monoid[Option[Int]].combine(Some(123), Some(456)))
    println(Monoid[Option[Int]].combine(None, Some(999)))
    println(Monoid[Option[Int]].combine(None, None))


    // syntax
    println(Option(1).combine(None))
    println(Option.empty[String].combine(None))
    println(Option("abc").combine(None))

    println("a" |+| "b")

    // Excercise: Add all the things
    println(add(List(1,2,3)))
    println(add(List(Option(1), Option(2), Option(3))))
    println(add(List(Order(1, 2), Order(2, 5), Order(3, 4))))
    println(add(List.empty[Order]))
  }
}