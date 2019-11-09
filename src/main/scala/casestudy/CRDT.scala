package casestudy
import scala.util.Try

import cats.instances.map._
import cats.instances.int._
import cats.instances.set._
import cats.syntax.monoid._
import cats.kernel.CommutativeMonoid

// Commutative Replicated Data Types
object CRDT {


  // 1. Abstract values to a BoundedSemiLattice
  trait BoundedSemiLattice[A] {
    def combine(x: A, y: A): A
    def empty: A
  }

  implicit val intInstance = new BoundedSemiLattice[Int] {
    def combine(x: Int, y: Int): Int = x max y
    def empty: Int = 0
  }

  implicit def setInstance[A] = new BoundedSemiLattice[Set[A]] {
    def combine(x: Set[A], y: Set[A]): Set[A] = x | y
    def empty: Set[A] = Set.empty[A]
  }

  // 2. abstract map to a Key-Value store
  trait KeyValueStore[F[_,_], K, V] {
    def put(f: F[K, V])(k: K, v: V): F[K, V]
    def get(f: F[K, V])(k: K): Option[V]
    def getOrElse(f: F[K, V])(k: K, default: V): V
    def values(f: F[K, V]): Iterable[V]
    def keys(f: F[K, V]): Iterable[K]
    def keyValues(f: F[K, V]): Iterable[(K, V)]
  }

  // kv instance for Map
  implicit def mapInstance[K, V] = new KeyValueStore[Map, K, V]{
    def get(f: Map[K,V])(k: K): Option[V] = f.get(k)
    def getOrElse(f: Map[K,V])(k: K, default: V): V = f.getOrElse(k, default)
    def put(f: Map[K,V])(k: K, v: V): Map[K,V] = f + (k -> v)
    def values(f: Map[K,V]): Iterable[V] = f.values
    def keys(f: Map[K,V]): Iterable[K] = f.keys
    def keyValues(f: Map[K,V]): Iterable[(K, V)] = f.keys.zip(f.values)
  }

  // syntax for kvstore
  implicit class KVOps[F[_, _], K, V](f: F[K, V])(implicit kvs: KeyValueStore[F, K, V]) {
    def get(k: K): Option[V] = kvs.get(f)(k)
    def getOrElse(k: K, default: V): V = kvs.getOrElse(f)(k, default)
    def put(k: K, v: V): F[K,V] = kvs.put(f)(k, v)
    def values: Iterable[V] = kvs.values(f)
    def keys: Iterable[K] = kvs.keys(f)
    def keyValues = kvs.keyValues(f)
  }

  object KeyValueStore {
    def apply[F[_,_], K, V](implicit kvs: KeyValueStore[F, K, V]) = kvs
  }

  // G Counter
  trait GCounter[F[_,_], K, V] {
    def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]
    def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]
    def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
  }

  object GCounter {

    def apply[F[_,_], K, V](implicit g: GCounter[F, K, V]) = g

    // implicit instance for (F[_, _], K, V), if:
    //   1. there is an instance of KeyValueStore[F, K, V]
    //   2. there is an instance of CommutativeMonoid[F[K, V]] 
    //   3. there is an instance of CommutativeMonoid[V]
    implicit def gcounterInstance[F[_,_], K, V](
      implicit kvs: KeyValueStore[F, K, V], 
      km: CommutativeMonoid[F[K, V]],
      m: CommutativeMonoid[V],
      b: BoundedSemiLattice[V]
    ) = new GCounter[F, K, V] {
      def increment(f: F[K,V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V] = 
        f.put(k, v |+| f.getOrElse(k, m.empty))
      def merge(f1: F[K,V], f2: F[K,V])(implicit b: BoundedSemiLattice[V]): F[K,V] = 
        f2.keyValues.foldLeft(f1) { case (keyValueStore, (key, value)) => 
          val combinedValue = b.combine(value, f1.getOrElse(key, b.empty))
          keyValueStore.put(key, combinedValue)
        }
      def total(f: F[K,V])(implicit m: CommutativeMonoid[V]): V = 
        m.combineAll(f.values)
    }
  }

  // syntax for gcounter
  implicit class GCounter2Ops[F[_,_], K, V](f1: F[K, V])(implicit gCounter2: GCounter[F, K, V]) {
    def increment(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V] = gCounter2.increment(f1)(k, v)(m)
    def merge(f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] = gCounter2.merge(f1, f2)(b)
    def total(implicit m: CommutativeMonoid[V]): V = gCounter2.total(f1)(m)
  }

  // print helper
  implicit class Print[A](a: A) {
    def print: Unit = println(a.toString)
  }

  def main(args: Array[String]): Unit = {

    // string -> int store
    // increment
    Map("a" -> 5).increment("a", 3).print

    // total
    Map("a" -> 5, "b" -> 10).total.print

    // merge 
    Map("a" -> 5)
      .merge(Map("b" -> 4))
      .merge(Map("a" -> 2, "c" -> 10))
      .print


    // actually can use std monoid instance for set
    // string -> set store
    // increment
    Map("a" -> Set(1,2)).increment("b", Set(2,3)).print

    // total
    Map("a" -> Set(1,2,3), "c" -> Set(2,3,4)).total.print

    // merge 
    Map("a" -> Set(2, 3))
      .merge(Map("b" -> Set(4)))
      .merge(Map("a" -> Set(2, 4), "b" -> Set(5, 6), "c" -> Set(7, 8)))
      .print

  }

}