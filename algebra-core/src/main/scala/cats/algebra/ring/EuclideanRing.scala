package cats
package algebra
package ring

import scala.annotation.tailrec
import scala.{specialized => sp}

/**
 * EuclideanRing implements a Euclidean domain.
 *
 * The formal definition says that every euclidean domain A has (at
 * least one) euclidean function f: A -> N (the natural numbers) where:
 *
 *   (for every x and non-zero y) x = yq + r, and r = 0 or f(r) < f(y).
 *
 * This generalizes the Euclidean division of integers, where f represents
 * a measure of length (or absolute value), and the previous equation
 * represents finding the quotient and remainder of x and y. So:
 *
 *   quot(x, y) = q
 *   mod(x, y) = r
 */
trait EuclideanRing[@sp(Int, Long, Float, Double) A] extends Any with GCDRing[A] { self =>
  def euclideanFunction(a: A): BigInt
  def equot(a: A, b: A): A
  def emod(a: A, b: A): A
  def equotmod(a: A, b: A): (A, A) = (equot(a, b), emod(a, b))
  def gcd(a: A, b: A)(implicit ev: Eq[A]): A =
    EuclideanRing.euclid(a, b)(ev, self)
  def lcm(a: A, b: A)(implicit ev: Eq[A]): A =
    if (isZero(a) || isZero(b)) zero else times(equot(a, gcd(a, b)), b)
}

trait EuclideanRingFunctions[R[T] <: EuclideanRing[T]] extends GCDRingFunctions[R] {
  def euclideanFunction[@sp(Int, Long, Float, Double) A](a: A)(implicit ev: R[A]): BigInt =
    ev.euclideanFunction(a)
  def equot[@sp(Int, Long, Float, Double) A](a: A, b: A)(implicit ev: R[A]): A =
    ev.equot(a, b)
  def emod[@sp(Int, Long, Float, Double) A](a: A, b: A)(implicit ev: R[A]): A =
    ev.emod(a, b)
  def equotmod[@sp(Int, Long, Float, Double) A](a: A, b: A)(implicit ev: R[A]): (A, A) =
    ev.equotmod(a, b)
}

object EuclideanRing extends EuclideanRingFunctions[EuclideanRing] {

  @inline final def apply[A](implicit e: EuclideanRing[A]): EuclideanRing[A] = e

  /**
   * Simple implementation of Euclid's algorithm for gcd
   */
  @tailrec final def euclid[@sp(Int, Long, Float, Double) A: Eq: EuclideanRing](a: A, b: A): A = {
    if (EuclideanRing[A].isZero(b)) a else euclid(b, EuclideanRing[A].emod(a, b))
  }

}
