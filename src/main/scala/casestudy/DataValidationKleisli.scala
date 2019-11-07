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
import cats.data.Kleisli
import cats.instances.either._
import cats.instances.string._

final case class User(username: String, email: String)

object DataValidationKleisli {

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
    def run(implicit s: Semigroup[E]): A => Validated[E, A] = apply(_)
  }

  object Predicate {
    def apply[E, A](f: A => Validated[E, A]): Pure[E, A] = Pure(f)

    final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A] 
    final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
    final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]
  }

  def error(msg: String) = NonEmptyList.of(msg)

  def longerThan(n: Int) = Predicate { s: String => 
    if (s.size > n) s.valid
    else error(s"must be greater than $n").invalid
  }
  
  def alphaNumeric = Predicate { s: String =>
    if (s.forall(_.isLetterOrDigit)) s.valid
    else error(s"must be alphanumeric").invalid
  }

  def containOnce(c: Char) = Predicate { s: String =>
    if (s.filter(_ == c).size == 1) s.valid
    else error(s"must contain $c exactly once").invalid
  }

  def contain(c: Char) = Predicate { s: String =>
    if (s.contains(c)) s.valid
    else error(s"must contain $c exactly once").invalid
  }

  def main(args: Array[String]): Unit = {

    val usernameCheck = Kleisli( (longerThan(4) and alphaNumeric).run )
    val emailCheck = Kleisli( containOnce('@').run )
      .productL(Kleisli { s => 
        val n = s.indexOf('@')
        longerThan(0)(s.take(n)) |+| (longerThan(3) and contain('.'))(s.drop(n + 1))
      })

    usernameCheck.run("fda").toString().print

    emailCheck.run("dfadm").toString().print
    emailCheck.run("@dm").toString().print
    emailCheck.run("fdsa@ds.com").toString().print

    def createUser(username: String, email: String): Either[NonEmptyList[String], User] = 
      (usernameCheck.run(username), emailCheck.run(email))
        .mapN(User)
        .toEither

    createUser("d", "fd").toString().print
    createUser("d", "@fd").toString().print
    createUser("d", "@f.d").toString().print
    createUser("dingding", "cola@gmail.com").toString().print

    usernameCheck.run("fdaf")
      .product(emailCheck.run("fda.fads"))
      .map { case (username, email) => User(username, email)}
      .toString()
      .print

    usernameCheck.run("dingding")
      .product(emailCheck.run("cola@gmail.com"))
      .map { case (username, email) => User(username, email)}
      .toString()
      .print
  }

}