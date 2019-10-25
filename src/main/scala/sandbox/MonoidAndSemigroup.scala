package sandbox

import scala.collection.immutable.Set

object MonoidAndSemigroup {

  // ** Semigroup **

  // A Semigroup for a type A is:
  //   - an operation *combine* with type (A, A) => A

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  def associativeLawSemigroup[A](x: A, y: A, z: A)(implicit s: Semigroup[A]): Boolean = 
    s.combine(x, s.combine(y, z)) == s.combine(s.combine(x, y), z)



  // ** Monoid **

  // A monoid for a type A is:
  //   - an operation *combine* with type (A, A) => A
  //   - en element empty of type A

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  def associativeLawMonoid[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean = 
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)

  def identityLawMonoid[A](x: A)(implicit m: Monoid[A]) = 
    m.combine(x, m.empty) == x && m.combine(m.empty, x) == x


  object Monoid {
    def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid
    def pure[A](comb: (A, A) => A, emp: A) = new Monoid[A] {
      def combine(x: A, y: A): A = comb(x, y)
      def empty: A = emp
    }

  }

  object MonoidImpl {

    // ** Boolean **

    implicit val booleanAnd = Monoid.pure[Boolean](
      (a, b) => a && b,
      true
    )

    implicit val booleanOr = Monoid.pure[Boolean](
      (a, b) => a || b,
      false
    )

    // xor is also a monoid
    implicit val booleanXor = Monoid.pure[Boolean](
      (a, b) => a ^ b,
      false
    )

    // xnor is also a monoid
    implicit val booleanXnor = Monoid.pure[Boolean](
      (a, b) => !(a ^ b),
      true
    )

    // interestingly, nand is not a monoid nor semigroup
    // but, from nand all logic gates derive...

    
    // ** Set[T] **

    implicit def setUnion[T] = Monoid.pure[Set[T]](
      (x: Set[T], y: Set[T]) => x.union(y),
      Set.empty[T]
    )
  }

  object SemigroupImpl {
    implicit def setIntersection[T] = new Semigroup[Set[T]] {
      def combine(x: Set[T], y: Set[T]): Set[T] = x.intersect(y)
    }
  }

}