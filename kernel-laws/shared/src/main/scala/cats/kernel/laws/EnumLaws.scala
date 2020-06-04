package cats.kernel.laws

import cats.kernel.{Next, Previous, PartialPrevious, PartialNext}

trait PartialPreviousLaws[A] extends LowerBoundedLaws[A] {

  implicit def P: PartialPrevious[A]

  def previousMinBound: IsEq[Option[A]] =
    P.partialPrevious(P.minBound) <-> None

  def partialPreviousLessThan(x: A): IsEq[Boolean] =
    P.partialPrevious(x).map(E.lt(_, x)).getOrElse(true) <-> true

}

trait PreviousLaws[A] extends PartialOrderLaws[A] {

  implicit def P: Previous[A]

  def previousLessThan(x: A): IsEq[Boolean] =
    E.lt(P.previous(x) , x) <-> true
}

trait PartialNextLaws[A] extends UpperBoundedLaws[A] {

  implicit def N: PartialNext[A]

  def nextMaxBound: IsEq[Option[A]] =
    N.partialNext(N.maxBound) <-> None

  def partialNextGreaterThan(x: A): IsEq[Boolean] =
    N.partialNext(x).map(E.gt(_, x)).getOrElse(true) <-> true

}

trait NextLaws[A] extends PartialOrderLaws[A] {

  implicit def N: Next[A]

  def nextGreaterThan(x: A): IsEq[Boolean] =
    E.gt(N.next(x) , x) <-> true
}
