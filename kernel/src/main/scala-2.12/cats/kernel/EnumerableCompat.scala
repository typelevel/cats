package cats
package kernel

import scala.{specialized => sp}
import scala.collection.immutable.Stream

trait PartialPreviousUpperBounded[@sp A] extends PartialPrevious[A] with PartialNext[A] with UpperBounded[A] {

  /**
   * Enumerate the members in descending order.
   */
  def membersDescending: Stream[A] = {
    def loop(a: A): Stream[A] =
      partialPrevious(a) match {
        case Some(aa) => aa #:: loop(aa)
        case _        => Stream.empty
      }
    maxBound #:: loop(maxBound)
  }

}

trait PartialNextLowerBounded[@sp A] extends PartialPrevious[A] with PartialNext[A] with LowerBounded[A] {

  /**
   * Enumerate the members in ascending order.
   */
  def membersAscending: Stream[A] = {
    def loop(a: A): Stream[A] =
      partialNext(a) match {
        case Some(aa) => aa #:: loop(aa)
        case _        => Stream.empty
      }
    minBound #:: loop(minBound)
  }

}
