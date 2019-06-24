package cats.kernel

import scala.{specialized => sp}

/**
 * A type class used to name the lower limit of a type.
 */
trait LowerBounded[@sp A] extends PartialOrder[A] {

  /**
   * Returns the lower limit of a type.
   */
  def minBound: A
}

trait LowerBoundedFunctions[L[T] <: LowerBounded[T]] {
  def minBound[@sp A](implicit ev: L[A]): A = ev.minBound
}

object LowerBounded extends LowerBoundedFunctions[LowerBounded] {
  @inline def apply[A](implicit l: LowerBounded[A]): LowerBounded[A] = l
}

/**
 * A type class used to name the upper limit of a type.
 */
trait UpperBounded[@sp A] extends PartialOrder[A] {

  /**
   * Returns the upper limit of a type.
   */
  def maxBound: A
}

trait UpperBoundedFunctions[U[T] <: UpperBounded[T]] {
  def maxBound[@sp A](implicit ev: U[A]): A = ev.maxBound
}

object UpperBounded extends UpperBoundedFunctions[UpperBounded] {
  @inline def apply[A](implicit u: UpperBounded[A]): UpperBounded[A] = u
}

/**
 * A type class used to name both the upper and lower limits of a type.
 */
trait Bounded[@sp A] extends LowerBounded[A] with UpperBounded[A]

object Bounded extends LowerBoundedFunctions[Bounded] with UpperBoundedFunctions[Bounded] {
  @inline def apply[A](implicit b: Bounded[A]): Bounded[A] = b
}
