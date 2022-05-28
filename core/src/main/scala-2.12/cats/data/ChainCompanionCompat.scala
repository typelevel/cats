package cats.data

import cats.data.Chain.{nil, one, Wrap}
import cats.kernel.compat.scalaVersionSpecific.IterableOnce

import scala.collection.immutable.{Seq => ImSeq}
import scala.collection.mutable.{Seq => MutSeq}

private[data] trait ChainCompanionCompat {

  /**
   * Creates a Chain from the specified sequence.
   */
  def fromSeq[A](s: Seq[A]): Chain[A] =
    s match {
      case mut: MutSeq[A] => fromMutableSeq(mut)
      case imm: ImSeq[A]  => fromImmutableSeq(imm)
    }

  private def fromImmutableSeq[A](s: ImSeq[A]): Chain[A] = {
    if (s.isEmpty) nil
    else if (s.lengthCompare(1) == 0) one(s.head)
    else Wrap(s)
  }

  private def fromMutableSeq[A](s: MutSeq[A]): Chain[A] = {
    if (s.isEmpty) nil
    else if (s.lengthCompare(1) == 0) one(s.head)
    else Wrap(s.toVector)
  }

  /**
   * Creates a Chain from the specified IterableOnce.
   */
  def fromIterableOnce[A](xs: IterableOnce[A]): Chain[A] =
    xs match {
      case s: ImSeq[A]  => fromImmutableSeq(s) // pay O(1) not O(N) cost
      case s: MutSeq[A] => fromMutableSeq(s)
      case notSeq =>
        fromImmutableSeq(notSeq.toVector) // toSeq could return a Stream, creating potential race conditions
    }
}