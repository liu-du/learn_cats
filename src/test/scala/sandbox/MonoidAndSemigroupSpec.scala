package sandbox

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen

import sandbox.MonoidAndSemigroup.{
  Monoid, Semigroup, identityLawMonoid, associativeLawMonoid, associativeLawSemigroup
}
import sandbox.MonoidAndSemigroup.MonoidImpl._
import sandbox.MonoidAndSemigroup.SemigroupImpl._

abstract class SemigroupSpec[T](override val name: String)(
    implicit arb: Arbitrary[T],
             monoid: Semigroup[T]
  ) extends Properties(name) {
    property("associative law") = forAll { associativeLawSemigroup(_: T, _: T, _: T) }
}

abstract class MonoidSpec[T](override val name: String)(
    implicit arb: Arbitrary[T],
             monoid: Monoid[T]
  ) extends Properties(name) {
    def genSet[T](implicit gen: Gen[T]): Gen[Set[T]] = {
      Gen.containerOfN[Set, T](100, gen)
    }

    property("identity law") = forAll { identityLawMonoid(_: T) }
    property("associative law") = forAll { associativeLawMonoid(_: T, _: T, _: T) }
}

// pass in implicit arguments explicitly
object MonoidBooleanAndSpec  extends MonoidSpec("Monoid: Boolean under And")(arbBool, booleanAnd) 
object MonoidBooleanOrSpec   extends MonoidSpec("Monoid: Boolean under Or")(arbBool, booleanOr) 
object MonoidBooleanXorSpec  extends MonoidSpec("Monoid: Boolean under Xor")(arbBool, booleanXor)
object MonoidBooleanXnorSpec extends MonoidSpec("Monoid: Boolean under Xnor")(arbBool, booleanXnor)

object MonoidSetUnionSpec extends MonoidSpec[Set[Int]](
  "Monoid: Set under Union"
  )(arbContainer[Set, Int], setUnion: Monoid[Set[Int]])

object semigroupSetIntersectionSpec extends SemigroupSpec[Set[Int]](
  "Semigroup: Set under Intersection"
  )(arbContainer[Set, Int], setIntersection: Semigroup[Set[Int]])
