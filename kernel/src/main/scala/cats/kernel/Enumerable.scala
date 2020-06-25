package cats
package kernel

import scala.{specialized => sp}

/**
 * A typeclass with an operation which returns a member which is
 * greater or `None` than the one supplied.
 */
trait PartialNext[@sp A] {
  def partialOrder: PartialOrder[A]
  def partialNext(a: A): Option[A]
}

/**
 * A typeclass with an operation which returns a member which is
 * always greater than the one supplied.
 */
trait Next[@sp A] extends PartialNext[A] {
  def next(a: A): A
  override def partialNext(a: A): Option[A] = Some(next(a))
}

/**
 * A typeclass with an operation which returns a member which is
 * smaller or `None` than the one supplied.
 */
trait PartialPrevious[@sp A] {
  def partialOrder: PartialOrder[A]
  def partialPrevious(a: A): Option[A]
}

/**
 * A typeclass with an operation which returns a member which is
 * always smaller than the one supplied.
 */
trait Previous[@sp A] extends PartialPrevious[A] {
  def partialOrder: PartialOrder[A]
  def previous(a: A): A
  override def partialPrevious(a: A): Option[A] = Some(previous(a))
}

/**
 * A typeclass which has both `previous` and `next` operations
 * such that `next . previous == identity`.
 */
trait UnboundedEnumerable[@sp A] extends Next[A] with Previous[A] {
  def order: Order[A]
  override def partialOrder: PartialOrder[A] = order
}

trait BoundedEnumerable[@sp A] extends PartialPreviousUpperBounded[A] with PartialNextLowerBounded[A] {

  def order: Order[A]
  override def partialOrder: PartialOrder[A] = order

  def cycleNext(a: A): A =
    partialNext(a).getOrElse(minBound)

  def cyclePrevious(a: A): A =
    partialPrevious(a).getOrElse(maxBound)

}

object BoundedEnumerable {
  implicit def catsKernelBoundedEnumerableForUnit: BoundedEnumerable[Unit] =
    cats.kernel.instances.unit.catsKernelStdOrderForUnit
  implicit def catsKernelBoundedEnumerableForBoolean: BoundedEnumerable[Boolean] =
    cats.kernel.instances.boolean.catsKernelStdOrderForBoolean
  implicit def catsKernelBoundedEnumerableForInt: BoundedEnumerable[Int] =
    cats.kernel.instances.int.catsKernelStdOrderForInt
  implicit def catsKernelBoundedEnumerableForShort: BoundedEnumerable[Short] =
    cats.kernel.instances.short.catsKernelStdOrderForShort
  implicit def catsKernelBoundedEnumerableForLong: BoundedEnumerable[Long] =
    cats.kernel.instances.long.catsKernelStdOrderForLong
  implicit def catsKernelBoundedEnumerableForChar: BoundedEnumerable[Char] =
    cats.kernel.instances.char.catsKernelStdOrderForChar
}

trait LowerBoundedEnumerable[@sp A] extends PartialNextLowerBounded[A] with Next[A] {
  def order: Order[A]
  override def partialOrder: PartialOrder[A] = order
}

trait UpperBoundedEnumerable[@sp A] extends PartialPreviousUpperBounded[A] with Previous[A] {
  def order: Order[A]
  override def partialOrder: PartialOrder[A] = order
}
