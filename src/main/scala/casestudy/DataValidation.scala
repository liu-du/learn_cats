package casestudy

import cats.Semigroup
import cats.syntax.either._
import cats.syntax.semigroup._
import sandbox.PrintableInstances._
import sandbox.PrintableSyntax._
import cats.instances.list._
import cats.data.Validated
import cats.syntax.apply._
import cats.data.Validated.Valid
import cats.data.Validated.Invalid
import cats.syntax.`package`.validated
import java.util.function.Predicate
import cats.syntax.validated._
import cats.data.NonEmptyList

object DataValidation {

  // Predicate

  sealed trait Predicate[E, A] {
    import Predicate._

    def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)
    def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match { 
      case Pure(func) => func(a)
      case And(left, right) => (left(a), right(a)).mapN((_, _) => a)
      case Or(left, right) => (left(a), right(a)) match {
        case (Invalid(e1), Invalid(e2)) => Invalid(e1 |+| e2)
        case _ => Valid(a)
      }
    }
    def run(implicit s: Semigroup[E]): A => Either[E, A] = apply(_).toEither
  }

  object Predicate {
    def apply[E, A](f: A => Validated[E, A]): Pure[E, A] = Pure(f)

    final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A] 
    final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
    final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]
  }

  // import Predicate._

  sealed trait Check[E, A, B] {
    import Check._
    def apply(a: A): Validated[E, B] 
    def map[C](f: B => C): Check[E, A, C] = MapCheck(this, f)
    def flapMap[C](f: B => Check[E, A, C]) = FlatMapCheck(this, f)
    def andThen[C](c: Check[E, B, C]) = AndThenCheck[E, A, B, C](this, c)
  }

  object Check {
    def apply[E: Semigroup, A](pred: Predicate[E, A]): Check[E, A, A] = PureCheck(pred)
    def apply[E: Semigroup, A](func: A => Validated[E, A]): Check[E, A, A] = apply(Predicate.Pure(func))

    final case class MapCheck[E, A, B, C](check: Check[E, A, B], func: B => C) extends Check[E, A, C] {
      def apply(a: A): Validated[E,C] = check(a).map(func)
    }
    final case class PureCheck[E: Semigroup, A](pred: Predicate[E, A]) extends Check[E, A, A] {
      def apply(a: A): Validated[E,A] = pred(a)
    }
    final case class FlatMapCheck[E, A, B, C](check: Check[E, A, B], func: B => Check[E, A, C]) extends Check[E, A, C] {
      def apply(a: A): Validated[E,C] = check(a).andThen(func(_)(a))
    }
    final case class AndThenCheck[E, A, B, C](check1: Check[E, A, B], check2: Check[E, B, C]) extends Check[E, A, C] {
      def apply(a: A): Validated[E,C] = check1(a).andThen(check2(_))
    }
  }

  def main(args: Array[String]): Unit = {

    val gt2 = Predicate { v: Int =>
      if (v > 2) v.valid
      else List("Must be > 2").invalid
    }
    
    val lt2 = Predicate { v: Int =>
      if (v < -2) v.valid
      else List("Must be < -2").invalid
    }
    
    val even = Predicate { v: Int =>
      if (v % 2 == 0) v.valid
      else List("Must be even number").invalid
    }
    
    val odd = Predicate { v: Int =>
      if (v % 2 != 0) v.valid
      else List("Must be odd number").invalid
    }
    
    gt2(3).toString().print
    lt2(3).toString().print
    (gt2 and lt2).apply(1).toString().print
    
    (gt2 or lt2).apply(1).toString().print
    (gt2 or lt2).apply(10).toString().print
    (gt2 or lt2).apply(-10).toString().print

    Check(gt2 or lt2)
      .map(_ * 10)
      .apply(4)
      .toString()
      .print

    Check(gt2 or lt2)
      .map(_ * 10)
      .apply(1)
      .toString()
      .print

    Check(gt2)
      .flapMap(int => if (int > 100) Check(even) else Check(odd))
      .apply(101)
      .toString()
      .print

    Check(gt2)
      .flapMap(int => if (int > 100) Check(even) else Check(odd))
      .apply(102)
      .toString()
      .print

    Check(gt2)
      .flapMap(int => if (int > 100) Check(even) else Check(odd))
      .apply(50)
      .toString()
      .print

    Check(gt2)
      .flapMap(int => if (int > 100) Check(even) else Check(odd))
      .apply(51)
      .toString()
      .print

    Check(gt2)
      .andThen(Check(even))
      .apply(50)
      .toString()
      .print


    type Errors = NonEmptyList[String]

    def error(s: String): NonEmptyList[String] = NonEmptyList.of(s)

    def longerThan(n: Int): Predicate[Errors, String] = Predicate { s => 
      if (s.size > n) Validated.valid(s)
      else Validated.invalid(error(s"must be longer than $n characters"))
    }

    def alphanumeric: Predicate[Errors, String] = Predicate { s =>
      if (s.forall(_.isLetterOrDigit)) Validated.valid(s)
      else Validated.invalid(error(s"must be all alpha numeric"))
    }

    def contains(char: Char): Predicate[Errors, String] = Predicate { s => 
      if (s.contains(char)) Validated.valid(s)
      else Validated.invalid(error(s"must contain $char"))
    }

    def containsOnce(char: Char): Predicate[Errors, String] = Predicate { s =>
      if (s.filter(_ == char).size == 1) Validated.valid(s)
      else Validated.invalid(error(s"must contain $char exactly once"))
    }
      
    val checkUsername = Check(longerThan(4) and alphanumeric)
    val checkEmail = 
      Check(containsOnce('@'))
        .map {s =>
          val i = s.indexOf('@')
          (s.take(i), s.drop(i + 1))
        }
        .andThen(Check{ 
          x: (String, String) => 
            (
              Check(longerThan(0)).apply(x._1), 
              Check(longerThan(3) and contains('.')).apply(x._2)
            ).mapN((_, _))
        })
        .map { case (x, y) => x + "@" + y }

    checkUsername("fsa").toString().print
    checkUsername("fsads").toString().print
    checkEmail("@dsa").toString().print
    checkEmail("d@dsa").toString().print
    checkEmail("d@dsa.fds").toString().print

  }
}