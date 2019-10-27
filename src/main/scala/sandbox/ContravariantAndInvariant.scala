package sandbox

import cats.Contravariant
import cats.syntax.contravariant._
import cats.Show
import cats.instances.string._
import cats.syntax.show._

import PrintableSyntax._
import PrintableInstances._

import cats.Monoid
import cats.syntax.invariant._
import cats.syntax.monoid._

object ContravariantAndInvariant {
  def main(args: Array[String]): Unit = {

    println(
      Show[String].contramap[Symbol](s => s"'${s.name}").show(Symbol("dave"))
    )

    Printable[String].contramap[Symbol](s => s"'${s.name}").print(Symbol("date"))


    implicit val printablSymbol = Printable[String].contramap[Symbol](s => s"'${s.name}")
    implicit val symbolMonoid: Monoid[Symbol] = Monoid[String].imap(Symbol(_))(_.name)

    (Symbol("dave") |+| Symbol(" ") |+| Symbol("robert")).print
  }
}