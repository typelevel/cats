package cats
package kernel

import scala.annotation.tailrec
import scala.{specialized => sp}

/**
 * A typeclass with an operation which returns a member which is
 * smaller or `None` than the one supplied.
 */
trait PartialPrevious[@sp A] {
  def partialOrder: PartialOrder[A]
  def partialPrevious(a: A): Option[A]

  /** As [[#partialPrevious]], but rather than getting the previous element, it gets the
    * Nth previous element.
    */
  def partialPreviousBy(a: A, n: BigInt): Option[A] = {
    val Zero: BigInt = BigInt(0)
    val One: BigInt = BigInt(1)

    @tailrec
    def loop(acc: A, n: BigInt): Option[A] =
      if (n <= Zero) {
        Some(acc)
      } else {
        partialPrevious(acc) match {
          case Some(acc) =>
            loop(acc, n - One)
          case otherwise =>
            otherwise
        }
      }

    loop(a, n)
  }
}

object PartialPrevious {

  def apply[A](implicit A: PartialPrevious[A]): PartialPrevious[A] = A

  def reverse[A](partialPrevious: PartialPrevious[A]): PartialNext[A] =
    new PartialNext[A] {
      override val partialOrder: PartialOrder[A] =
        PartialOrder.reverse(partialPrevious.partialOrder)
      override def partialNext(a: A): Option[A] =
        partialPrevious.partialPrevious(a)
    }

  implicit def catsKernelPartialPreviousForBigInt: PartialPrevious[BigInt] =
    cats.kernel.instances.bigInt.catsKernelStdOrderForBigInt
  implicit def catsKernelPartialPreviousForInt: PartialPrevious[Int] =
    cats.kernel.instances.int.catsKernelStdBoundableEnumerableForInt
  implicit def catsKernelPartialPreviousForUnit: PartialPrevious[Unit] =
    cats.kernel.instances.unit.catsKernelStdBoundableEnumerableForUnit
  implicit def catsKernelPartialPreviousForBoolean: PartialPrevious[Boolean] =
    cats.kernel.instances.boolean.catsKernelStdBoundableEnumerableForBoolean
  implicit def catsKernelPartialPreviousForByte: PartialPrevious[Byte] =
    cats.kernel.instances.byte.catsKernelStdBoundableEnumerableForByte
  implicit def catsKernelPartialPreviousForShort: PartialPrevious[Short] =
    cats.kernel.instances.short.catsKernelStdBoundableEnumerableForShort
  implicit def catsKernelPartialPreviousForLong: PartialPrevious[Long] =
    cats.kernel.instances.long.catsKernelStdBoundableEnumerableForLong
  implicit def catsKernelPartialPreviousForChar: PartialPrevious[Char] =
    cats.kernel.instances.char.catsKernelStdBoundableEnumerableForChar
}
