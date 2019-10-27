package sandbox

import cats.Functor
import cats.instances.list._
// import cats.instances.option._
import cats.instances.function._
import cats.syntax.functor._
import sandbox.CatsFunctor.Branch
import sandbox.CatsFunctor.Leaf

object CatsFunctor {

  // custom option functor instance
  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = {
      println("custom option map")
      fa.map(f)
    }
  }

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] 
  final case class Leaf[A](value: A) extends Tree[A]
  

  def main(args: Array[String]): Unit = {
    val func1: Int => Double = x => (x * 2).toDouble
    val func2: Double => String = _.toString() * 2

    println((func1 andThen func2)(1))
    println((func1 map func2)(2))
    println(func2(func1(3)))

    // R style %>%
    (() => 4)
      .map(func1)
      .map(func2)
      .fmap(println)
      .apply()


    val list1 = List(1,2,3)

    // List functor
    println(Functor[List].map(list1)(_ * 2))


    val option1 = Option(123)
    println(Functor[Option].map(option1)(_.toString))

    val func = (x: Int) => x + 1
    val liftedFunc = Functor[Option].lift(func)
    println(liftedFunc(Option(456)))

    def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] = {
      start.map(n => n + 2)
    }

    println(doMath(Option(123)))
    println(doMath(List(123, 345)))

    println(Functor[Option].map(Option(999))(_ * 2))


    // Exercise 3.5.4
    //     tree 
    //     /  \
    //    / \  3
    //   1  2
    implicit val treeFunctor = new Functor[Tree] {
      def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
        case Leaf(value) => Leaf(f(value))
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      }
    }

    // cats.Functor is invariant so need to define instances for subtypes
    implicit val branchFunctor = new Functor[Branch] {
      def map[A, B](fa: Branch[A])(f: A => B): Branch[B] = fa match {
        case Branch(left, right) => Branch(left.map(f), right.map(f))
      }
    }

    implicit val leafFunctor = new Functor[Leaf] {
      def map[A, B](fa: Leaf[A])(f: A => B): Leaf[B] = Leaf(f(fa.value))
    }

    val tree: Tree[Int] = Branch(Branch(Leaf(1), 
                                        Leaf(2)), 
                                 Leaf(3))

    println(tree.map(_ * 10))

    val branch = Branch(Branch(Leaf(1), 
                               Leaf(2)), 
                        Leaf(3))

    println(branch.map(_ * 100))

    val leaf = Leaf(123)
    println(leaf.map(_ * 1000))


  }

}