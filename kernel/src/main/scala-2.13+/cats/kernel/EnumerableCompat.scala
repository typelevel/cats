package cats
package kernel

import scala.{specialized => sp}
import scala.collection.immutable.LazyList

trait PartialPreviousUpperBounded[@sp A] extends PartialPrevious[A] with PartialNext[A] with UpperBounded[A] {

  /**
   * Enumerate the members in descending order.
   */
  def membersDescending: LazyList[A] = {
    def loop(a: A): LazyList[A] =
      partialPrevious(a) match {
        case Some(aa) => aa #:: loop(aa)
        case _        => LazyList.empty
      }
    maxBound #:: loop(maxBound)
  }

}

trait PartialNextLowerBounded[@sp A] extends PartialPrevious[A] with PartialNext[A] with LowerBounded[A] {

  /**
   * Enumerate the members in ascending order.
   */
  def membersAscending: LazyList[A] = {
    def loop(a: A): LazyList[A] =
      partialNext(a) match {
        case Some(aa) => aa #:: loop(aa)
        case _        => LazyList.empty
      }
    minBound #:: loop(minBound)
  }

}
