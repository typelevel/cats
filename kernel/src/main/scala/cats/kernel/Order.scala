package cats.kernel

import scala.{specialized => sp}

/**
 * The `Order` type class is used to define a total ordering on some type `A`.
 * An order is defined by a relation <=, which obeys the following laws:
 *
 * - either x <= y or y <= x (totality)
 * - if x <= y and y <= x, then x == y (antisymmetry)
 * - if x <= y and y <= z, then x <= z (transitivity)
 *
 * The truth table for compare is defined as follows:
 *
 * x <= y    x >= y      Int
 * true      true        = 0     (corresponds to x == y)
 * true      false       < 0     (corresponds to x < y)
 * false     true        > 0     (corresponds to x > y)
 *
 * By the totality law, x <= y and y <= x cannot be both false.
 */
trait Order[@sp A] extends Any with PartialOrder[A] { self =>

  /**
   * Result of comparing `x` with `y`. Returns an Int whose sign is:
   * - negative iff `x < y`
   * - zero     iff `x = y`
   * - positive iff `x > y`
   */
  def compare(x: A, y: A): Int

  /**
   * Like `compare`, but returns a [[cats.kernel.Comparison]] instead of an Int.
   * Has the benefit of being able to pattern match on, but not as performant.
   */
  def comparison(x: A, y: A): Comparison = Comparison.fromInt(compare(x, y))

  def partialCompare(x: A, y: A): Double = compare(x, y).toDouble

  /**
   * If x < y, return x, else return y.
   */
  def min(x: A, y: A): A = if (lt(x, y)) x else y

  /**
   * If x > y, return x, else return y.
   */
  def max(x: A, y: A): A = if (gt(x, y)) x else y

  // The following may be overridden for performance:

  /**
   * Returns true if `x` = `y`, false otherwise.
   */
  override def eqv(x: A, y: A): Boolean =
    compare(x, y) == 0

  /**
   * Returns true if `x` != `y`, false otherwise.
   *
   * Note: this default implementation provided by [[Order]] is the same as the
   * one defined in [[Eq]], but for purposes of binary compatibility, the
   * override in [[Order]] has not yet been removed.
   * See [[https://github.com/typelevel/cats/pull/2230#issuecomment-381818633 this discussion]].
   */
  override def neqv(x: A, y: A): Boolean = !eqv(x, y)

  /**
   * Returns true if `x` <= `y`, false otherwise.
   */
  override def lteqv(x: A, y: A): Boolean =
    compare(x, y) <= 0

  /**
   * Returns true if `x` < `y`, false otherwise.
   */
  override def lt(x: A, y: A): Boolean =
    compare(x, y) < 0

  /**
   * Returns true if `x` >= `y`, false otherwise.
   */
  override def gteqv(x: A, y: A): Boolean =
    compare(x, y) >= 0

  /**
   * Returns true if `x` > `y`, false otherwise.
   */
  override def gt(x: A, y: A): Boolean =
    compare(x, y) > 0

  /**
   * Convert a `Order[A]` to a `scala.math.Ordering[A]`
   * instance.
   */
  def toOrdering: Ordering[A] = new Ordering[A] {
    def compare(x: A, y: A): Int = self.compare(x, y)
  }
}

abstract class OrderFunctions[O[T] <: Order[T]] extends PartialOrderFunctions[O] {

  def compare[@sp A](x: A, y: A)(implicit ev: O[A]): Int =
    ev.compare(x, y)

  def min[@sp A](x: A, y: A)(implicit ev: O[A]): A =
    ev.min(x, y)

  def max[@sp A](x: A, y: A)(implicit ev: O[A]): A =
    ev.max(x, y)

  def comparison[@sp A](x: A, y: A)(implicit ev: O[A]): Comparison =
    ev.comparison(x, y)
}

trait OrderToOrderingConversion {

  /**
   * Implicitly derive a `scala.math.Ordering[A]` from a `Order[A]`
   * instance.
   */
  implicit def catsKernelOrderingForOrder[A](implicit ev: Order[A]): Ordering[A] =
    ev.toOrdering

}

object Order extends OrderFunctions[Order] with OrderToOrderingConversion {

  /**
   * Access an implicit `Order[A]`.
   */
  @inline final def apply[A](implicit ev: Order[A]): Order[A] = ev

  /**
   * Convert an implicit `Order[B]` to an `Order[A]` using the given
   * function `f`.
   */
  def by[@sp A, @sp B](f: A => B)(implicit ev: Order[B]): Order[A] =
    new Order[A] {
      def compare(x: A, y: A): Int = ev.compare(f(x), f(y))
    }

  /**
   * Defines an ordering on `A` from the given order such that all arrows switch direction.
   */
  def reverse[@sp A](order: Order[A]): Order[A] =
    new Order[A] {
      def compare(x: A, y: A): Int = order.compare(y, x)
    }

  /**
   * Returns a new `Order[A]` instance that first compares by the first
   * `Order` instance and uses the second `Order` instance to "break ties".
   *
   * That is, `Order.whenEqual(x, y)` creates an `Order` that first orders by `x` and
   * then (if two elements are equal) falls back to `y` for the comparison.
   */
  def whenEqual[@sp A](first: Order[A], second: Order[A]): Order[A] =
    new Order[A] {
      def compare(x: A, y: A) = {
        val c = first.compare(x, y)
        if (c == 0) second.compare(x, y)
        else c
      }
    }

  /**
   * Define an `Order[A]` using the given function `f`.
   */
  def from[@sp A](f: (A, A) => Int): Order[A] =
    new Order[A] {
      def compare(x: A, y: A) = f(x, y)
    }

  /**
   * Define an `Order[A]` using the given 'less than' function `f`.
   */
  def fromLessThan[@sp A](f: (A, A) => Boolean): Order[A] =
    new Order[A] {
      override def compare(x: A, y: A): Int =
        if (f(x, y)) -1 else if (f(y, x)) 1 else 0

      // Overridden for performance (avoids multiple comparisons)
      override def eqv(x: A, y: A): Boolean = !(f(x, y) || f(y, x))
      override def neqv(x: A, y: A): Boolean = f(x, y) || f(y, x)
      override def lteqv(x: A, y: A): Boolean = !f(y, x)
      override def lt(x: A, y: A): Boolean = f(x, y)
      override def gteqv(x: A, y: A): Boolean = !f(x, y)
      override def gt(x: A, y: A): Boolean = f(y, x)
    }

  /**
   * An `Order` instance that considers all `A` instances to be equal.
   */
  def allEqual[A]: Order[A] =
    new Order[A] {
      def compare(x: A, y: A): Int = 0
    }

  /**
   * A `Monoid[Order[A]]` can be generated for all `A` with the following
   * properties:
   *
   * `empty` returns a trivial `Order[A]` which considers all `A` instances to
   * be equal.
   *
   * `combine(x: Order[A], y: Order[A])` creates an `Order[A]` that first
   * orders by `x` and then (if two elements are equal) falls back to `y`.
   *
   * This monoid is also a `Band[Order[A]]` since its combine
   * operations is idempotent.
   *
   * @see [[Order.whenEqual]]
   */
  def whenEqualMonoid[A]: Monoid[Order[A]] with Band[Order[A]] =
    new Monoid[Order[A]] with Band[Order[A]] {
      val empty: Order[A] = allEqual[A]
      def combine(x: Order[A], y: Order[A]): Order[A] = Order.whenEqual(x, y)
    }

  def fromOrdering[A](implicit ev: Ordering[A]): Order[A] =
    new Order[A] {
      def compare(x: A, y: A): Int = ev.compare(x, y)

      override def toOrdering: Ordering[A] = ev
    }

  def fromComparable[A <: Comparable[A]]: Order[A] =
    new Order[A] {
      override def compare(x: A, y: A): Int =
        x.compareTo(y)
    }
}
