package cats.data

import cats.data.Chain.{nil, one, Wrap}

private[data] trait ChainCompanionCompat {

  /**
   * Creates a Chain from the specified sequence.
   */
  def fromSeq[A](s: Seq[A]): Chain[A] =
    if (s.isEmpty) nil
    else if (s.lengthCompare(1) == 0) one(s.head)
    else Wrap(s)

  /**
   * Creates a Chain from the specified IterableOnce.
   */
  def fromIterableOnce[A](xs: IterableOnce[A]): Chain[A] = Chain.fromSeq(
    xs match {
      case s: Seq[A] => s // pay O(1) not O(N) cost
      case notSeq    => notSeq.iterator.to(Vector) // toSeq could return a LazyList, creating potential race conditions
    }
  )
}
