package sandbox

import cats.Monad
import scala.annotation.tailrec
import cats.syntax.applicative._
import cats.syntax.monad._
import cats.syntax.functor._
import cats.syntax.flatMap._
import sandbox.MyCatMonad.Branch
import sandbox.MyCatMonad.Leaf

object MyCatMonad {

  val optionMonad = new Monad[Option] {
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
      case None => None
      case Some(value) => f(value)
    }
    def pure[A](x: A): Option[A] = Some(x)

    @tailrec
    def tailRecM[A, B](a: A)(f: A => Option[Either[A,B]]): Option[B] = f(a) match {
      case None => None
      case Some(Left(l)) => tailRecM(l)(f)
      case Some(Right(r)) => Some(r)
    }
  }

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)

  implicit val treeMonad = new Monad[Tree] {
    def pure[A](x: A): Tree[A] = leaf(x)
    def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Branch(left, right) => branch(flatMap(left)(f), flatMap(right)(f))
      case Leaf(value) => f(value)
    }
    // @tailrec
    def tailRecM[A, B](a: A)(f: A => Tree[Either[A,B]]): Tree[B] = {
      def helper(x: Tree[Either[A, B]]): Tree[B] = x match {
        case Leaf(Right(r)) => leaf(r)
        case Leaf(Left(l)) => tailRecM(a)(f)
        case Branch(left, right) => branch(helper(left), helper(right))
      }

      helper(f(a))
    }
  }

  def main(args: Array[String]): Unit = {

    val mytree = 1.pure[Tree]
    val mytree2 = mytree
      .flatMap(x => branch(leaf(x), leaf(x * 2)))
      .flatMap(x => branch(leaf(x * 10), leaf(x * 100)))
      .map(-_)

    println(mytree2)

    val mytree3 = for {
      a <- branch(leaf(1), leaf(-1))
      b <- branch(leaf(a * 10), leaf(a * 20))
      c <- branch(leaf(b * 10), leaf(b * 20))
    } yield c

    println(mytree3)

  }
}