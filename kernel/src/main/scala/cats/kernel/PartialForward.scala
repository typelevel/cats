package cats
package kernel

import scala.{specialized => sp}
import scala.annotation.tailrec

trait PartialForward[@sp A] extends Serializable {
  def partialOrder: PartialOrder[A]

  def partialNext(a: A): Option[A]

  /** As [[#partialNext]], but rather than getting the next element, it gets the
    * Nth next element.
    */
  def partialNextBy(a: A, n: BigInt): Option[A] = {
    val Zero: BigInt = BigInt(0)
    val One: BigInt=  BigInt(1)

    @tailrec
    def loop(acc: A, n: BigInt): Option[A] =
      if (n <= Zero) {
        Some(acc)
      } else {
        partialNext(acc) match {
          case Some(acc) =>
            loop(acc, n - One)
          case otherwise =>
            otherwise
        }
      }

    loop(a, n)
  }

  /** As [[#nextOrMin]], but steps forward N steps rather than 1 step.
    *
    * @note If this wraps around to the `minBound`, and there are still steps
    *       to apply, it will not stop at `minBound`. For example,
    *       `nextOrMinBy(Byte.MaxValue, 2) == -127`.
    */
  def nextOrMinBy(a: A, n: BigInt)(implicit A: LowerBounded[A]): A = {
    val Zero: BigInt = BigInt(0)
    val One: BigInt=  BigInt(1)

    @tailrec
    def loop(acc: A, n: BigInt): A =
      if (n <= Zero) {
        acc
      } else {
        partialNext(acc) match {
          case Some(acc) =>
            loop(acc, n - One)
          case _ =>
            loop(A.minBound, n - One)
        }
      }

    loop(a, n)
  }
}
