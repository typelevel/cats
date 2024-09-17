package cats
package kernel

import cats.kernel.{ScalaVersionSpecificLazyListCompat => LazyListLike}
import scala.annotation.tailrec
import scala.{specialized => sp}

trait UpperBoundedPartialPrevious[@sp A] extends PartialPrevious[A] with UpperBounded[A] {
  // Note this class provides no abstract methods. In fact, an argument can be
  // made that it need not exist at all, and that these methods should live on
  // PartialPrevious with a constraint at A has an UpperBounded[A]
  // instance. The reason that this class exists is because the Order family
  // of classes define a function on their companion object called `reverse`,
  // which changes the ordering and direction of all operations. For this to
  // be consistent, we need a class which extends both PartialPrevious and
  // UpperBounded so that when we reverse PartialPrevious, we also reverse
  // UpperBounded.

  /** As [[#previousOrMax]], but steps backward N steps rather than 1 step.
    *
    * @note If this wraps around to the `maxBound`, and there are still steps
    *       to apply, it will not stop at `maxBound`. For example,
    *       `previousOrMaxBy(Byte.MinValue, 2) == 126`.
    */
  def previousOrMaxBy(a: A, n: BigInt): A = {
    val Zero: BigInt = BigInt(0)
    val One: BigInt=  BigInt(1)

    @tailrec
    def loop(acc: A, n: BigInt): A =
      if (n <= Zero) {
        acc
      } else {
        partialPrevious(acc) match {
          case Some(acc) =>
            loop(acc, n - One)
          case _ =>
            loop(maxBound, n - One)
        }
      }

    loop(a, n)
  }

  /** Get the previous value if defined, otherwise get the maxBound. */
  def previousOrMax(a: A): A =
    previousOrMaxBy(a, BigInt(1))

  /** Create an infinite cycling lazy list starting from the given value with
    * each subsequent value N steps behind of the last. When there is no previous
    * value, e.g. `partialPrevious` returns `None`, restart the cycle from the
    * maxBound.
    */
  def cycleBackwardFromBy(start: A, n: BigInt): LazyListLike.T[A] =
    start #:: cycleBackwardFromBy(previousOrMaxBy(start, n), n)

  /** Create an infinite cycling lazy list starting from the given value. When
    * there is no previous value, e.g. `partialPrevious` returns `None`, restart the
    * cycle from the maxBound.
    *
    * @note This will only enumerate all the elements of the set if this type
    *       is a [[BoundableEnumerable]]. If the type is a
    *       [[BoundlessEnumerable]] then the cycle will never restart. If the
    *       type is neither, then it is possible the first cycle will
    *       enumerate elements that are not in following cycles. That is, if
    *       `previousOrMin` starting from `maxBound` yields `None` before reaching
    *       `start`, `start` and elements following `start` will only be in
    *       the first cycle. This is not possible for [[BoundableEnumerable]]
    *       or [[BoundlessEnumerable]] types, but may be possible for types
    *       which only have a [[PartialPrevious]] instance.
    */
  def cycleBackwardFrom(start: A): LazyListLike.T[A] =
    cycleBackwardFromBy(start, BigInt(1))

  /** As [[#cycleBackwardFromByN]], but uses the maxBound as the start value. */
  def cycleBackwardBy(n: BigInt): LazyListLike.T[A] =
    cycleBackwardFromBy(maxBound, n)

  /** As [[#cycleBackwardFrom]], but uses the maxBound as the start value.
    *
    * Because this cycle starts at the maxBound, each cycle will have the same
    * elements in it. However, as with [[#cycleBackwardFrom]], each cycle will
    * only contain all elements of `A` if `A` is either a
    * [[BoundableEnumerable]] or [[BoundlessEnumerable]] type.
    */
  def cycleBackward: LazyListLike.T[A] =
    cycleBackwardFrom(maxBound)
}

object UpperBoundedPartialPrevious {
  def apply[A](implicit A: UpperBoundedPartialPrevious[A]): UpperBoundedPartialPrevious[A] = A

  def reverse[A](partialPrevious: UpperBoundedPartialPrevious[A]): LowerBoundedPartialNext[A] =
    new PartialNext[A] {
      override val partialOrder: PartialOrder[A] =
        PartialOrder.reverse(partialPrevious.partialOrder)
      override def partialNext(a: A): Option[A] =
        partialPrevious.partialPrevious(a)
    }

  implicit def catsKernelUpperBoundedPartialPreviousForBigInt: UpperBoundedPartialPrevious[BigInt] =
    cats.kernel.instances.bigInt.catsKernelStdOrderForBigInt
  implicit def catsKernelUpperBoundedPartialPreviousForInt: UpperBoundedPartialPrevious[Int] =
    cats.kernel.instances.int.catsKernelStdBoundableEnumerableForInt
  implicit def catsKernelUpperBoundedPartialPreviousForUnit: UpperBoundedPartialPrevious[Unit] =
    cats.kernel.instances.unit.catsKernelStdBoundableEnumerableForUnit
  implicit def catsKernelUpperBoundedPartialPreviousForBoolean: UpperBoundedPartialPrevious[Boolean] =
    cats.kernel.instances.boolean.catsKernelStdBoundableEnumerableForBoolean
  implicit def catsKernelUpperBoundedPartialPreviousForByte: UpperBoundedPartialPrevious[Byte] =
    cats.kernel.instances.byte.catsKernelStdBoundableEnumerableForByte
  implicit def catsKernelUpperBoundedPartialPreviousForShort: UpperBoundedPartialPrevious[Short] =
    cats.kernel.instances.short.catsKernelStdBoundableEnumerableForShort
  implicit def catsKernelUpperBoundedPartialPreviousForLong: UpperBoundedPartialPrevious[Long] =
    cats.kernel.instances.long.catsKernelStdBoundableEnumerableForLong
  implicit def catsKernelUpperBoundedPartialPreviousForChar: UpperBoundedPartialPrevious[Char] =
    cats.kernel.instances.char.catsKernelStdBoundableEnumerableForChar
}
