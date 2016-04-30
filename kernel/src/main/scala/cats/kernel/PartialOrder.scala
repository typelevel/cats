package cats.kernel

import java.lang.Double.isNaN
import scala.{specialized => sp}

/**
 * The `PartialOrder` type class is used to define a partial ordering on some type `A`.
 * 
 * A partial order is defined by a relation <=, which obeys the following laws:
 * 
 * - x <= x (reflexivity)
 * - if x <= y and y <= x, then x = y (anti-symmetry)
 * - if x <= y and y <= z, then x <= z (transitivity)
 * 
 * To compute both <= and >= at the same time, we use a Double number
 * to encode the result of the comparisons x <= y and x >= y.
 * The truth table is defined as follows:
 * 
 * x <= y    x >= y      Double
 * true      true        = 0.0     (corresponds to x = y)
 * false     false       = NaN     (x and y cannot be compared)
 * true      false       = -1.0    (corresponds to x < y)
 * false     true        = 1.0     (corresponds to x > y)
 */
trait PartialOrder[@sp A] extends Any with Eq[A] { self =>

  /**
   * Result of comparing `x` with `y`. Returns NaN if operands are not
   * comparable. If operands are comparable, returns a Double whose
   * sign is:
   * - negative iff `x < y`
   * - zero     iff `x = y`
   * - positive iff `x > y`
   */
  def partialCompare(x: A, y: A): Double

  /**
   * Result of comparing `x` with `y`. Returns None if operands are
   * not comparable. If operands are comparable, returns Some[Int]
   * where the Int sign is:
   * - negative iff `x < y`
   * - zero     iff `x = y`
   * - positive iff `x > y`
   */  
  def tryCompare(x: A, y: A): Option[Int] = {
    val c = partialCompare(x, y)
    if (isNaN(c)) None else Some(c.signum)
  }

  /**
   * Returns Some(x) if x <= y, Some(y) if x > y, otherwise None.
   */
  def pmin(x: A, y: A): Option[A] = {
    val c = partialCompare(x, y)
    if (c <= 0) Some(x)
    else if (c > 0) Some(y)
    else None
  }

  /**
   * Returns Some(x) if x >= y, Some(y) if x < y, otherwise None.
   */
  def pmax(x: A, y: A): Option[A] = {
    val c = partialCompare(x, y)
    if (c >= 0)  Some(x)
    else if (c < 0) Some(y)
    else None
  }

  /**
   * Defines a partial order on `B` by mapping `B` to `A` using `f`
   * and using `A`s order to order `B`.
   */
  override def on[@sp B](f: B => A): PartialOrder[B] =
    new PartialOrder[B] {
      def partialCompare(x: B, y: B): Double = self.partialCompare(f(x), f(y))
    }

  /**
   * Defines a partial order on `A` where all arrows switch direction.
   */
  def reverse: PartialOrder[A] =
    new PartialOrder[A] {
      def partialCompare(x: A, y: A): Double = self.partialCompare(y, x)
    }

  // The following may be overridden for performance:

  /**
   * Returns true if `x` = `y`, false otherwise.
   */
  def eqv(x: A, y: A): Boolean = partialCompare(x, y) == 0

  /**
   * Returns true if `x` <= `y`, false otherwise.
   */
  def lteqv(x: A, y: A): Boolean = partialCompare(x, y) <= 0

  /**
   * Returns true if `x` < `y`, false otherwise.
   */
  def lt(x: A, y: A): Boolean = partialCompare(x, y) < 0

  /**
   * Returns true if `x` >= `y`, false otherwise.
   */
  def gteqv(x: A, y: A): Boolean = partialCompare(x, y) >= 0

  /**
   * Returns true if `x` > `y`, false otherwise.
   */
  def gt(x: A, y: A): Boolean = partialCompare(x, y) > 0
}

abstract class PartialOrderFunctions[P[T] <: PartialOrder[T]] extends EqFunctions[P] {

  def partialCompare[@sp A](x: A, y: A)(implicit ev: P[A]): Double =
    ev.partialCompare(x, y)
  def tryCompare[@sp A](x: A, y: A)(implicit ev: P[A]): Option[Int] =
    ev.tryCompare(x, y)

  def pmin[@sp A](x: A, y: A)(implicit ev: P[A]): Option[A] =
    ev.pmin(x, y)
  def pmax[@sp A](x: A, y: A)(implicit ev: P[A]): Option[A] =
    ev.pmax(x, y)

  def lteqv[@sp A](x: A, y: A)(implicit ev: P[A]): Boolean =
    ev.lteqv(x, y)
  def lt[@sp A](x: A, y: A)(implicit ev: P[A]): Boolean =
    ev.lt(x, y)
  def gteqv[@sp A](x: A, y: A)(implicit ev: P[A]): Boolean =
    ev.gteqv(x, y)
  def gt[@sp A](x: A, y: A)(implicit ev: P[A]): Boolean =
    ev.gt(x, y)
}

object PartialOrder extends PartialOrderFunctions[PartialOrder] {

  /**
   * Access an implicit `PartialOrder[A]`.
   */
  @inline final def apply[A](implicit ev: PartialOrder[A]) = ev

  /**
   * Convert an implicit `PartialOrder[B]` to an `PartialOrder[A]` using the given
   * function `f`.
   */
  def by[@sp A, @sp B](f: A => B)(implicit ev: PartialOrder[B]): PartialOrder[A] =
    ev.on(f)

  /**
   * Define a `PartialOrder[A]` using the given function `f`.
   */
  def from[@sp A](f: (A, A) => Double): PartialOrder[A] =
    new PartialOrder[A] {
      def partialCompare(x: A, y: A) = f(x, y)
    }

  /**
   * Implicitly convert a `PartialOrder[A]` to a
   * `scala.math.PartialOrdering[A]` instance.
   */
  implicit def partialOrdering[A](implicit ev: PartialOrder[A]): PartialOrdering[A] =
    new PartialOrdering[A] {
      def tryCompare(x: A, y: A): Option[Int] = ev.tryCompare(x, y)
      def lteq(x: A, y: A): Boolean = ev.lteqv(x, y)
    }
}
