package cats.kernel

import scala.{specialized => sp}
import scala.util.hashing.Hashing

/**
 * A type class used to represent a hashing scheme for objects of a given type.
 * For any two instances `x` and `y` that are considered equivalent under the
 * equivalence relation defined by this object, `hash(x)` should equal `hash(y)`.
 * @author Tongfei Chen
 */
trait Hash[@sp A] extends Any with Eq[A] with Serializable { self =>

  /**
   * Returns the hash code of the given object under this hashing scheme.
   */
  def hash(x: A): Int

  // `Hash#on` deliberately not implemented to avoid `Hash`/`Order` diamond inheritance problem.
  // Please use `Hash.by` for the same functionality.

  // `Hash#toHashing` deliberately not implemented since `scala.util.hashing.Hashing` is only
  // compatible with universal equality.
}

abstract class HashFunctions[H[T] <: Hash[T]] extends EqFunctions[H] {

  def hash[@sp A](x: A)(implicit ev: H[A]): Int = ev hash x

}


object Hash extends HashFunctions[Hash] {

  /** Fetch a `Hash` instance given the specific type. */
  @inline final def apply[A](implicit ev: Hash[A]): Hash[A] = ev

  def by[@sp A, @sp B](f: A => B)(implicit ev: Hash[B]): Hash[A] =
    new Hash[A] {
      def hash(x: A) = ev.hash(f(x))
      def eqv(x: A, y: A) = ev.eqv(f(x), f(y))
    }

  implicit def catsKernelHashingForHash[A](implicit ev: Hash[A]): Hashing[A] =
    new Hashing[A] {
      def hash(x: A): Int = ev.hash(x)
    }

  def fromHashing[A](implicit ev: Hashing[A]): Hash[A] =
    new Hash[A] {
      def hash(x: A) = ev.hash(x)
      def eqv(x: A, y: A) = x == y // universal equality
    }

  def fromUniversalHashCode[A]: Hash[A] =
    new Hash[A] {
      def hash(x: A) = x.##
      def eqv(x: A, y: A) = x == y
    }

}
