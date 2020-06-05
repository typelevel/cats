package cats.kernel
package laws

trait PartialPreviousLaws[A] extends PartialOrderLaws[A] {

  implicit def P: PartialPrevious[A]

  def previousOrderWeak(a: A): IsEq[Boolean] =
    P.partialPrevious(a).map(E.lt(_, a)).getOrElse(true) <-> true

  def previousOrderStrong(a: A, b: A): IsEq[Option[Boolean]] =
    if (E.gt(a, b)) {
      P.partialPrevious(a).map(E.gteqv(_, b)) <-> Option(true)
    } else {
      Option.empty <-> Option.empty
    }

}

object PartialPreviousLaws {
  def apply[A](implicit ev: PartialPrevious[A]): PartialPreviousLaws[A] =
    new PartialPreviousLaws[A] {
      def E: PartialOrder[A] = ev.partialOrder
      def P: PartialPrevious[A] = ev
    }
}

trait PartialNextLaws[A] extends PartialOrderLaws[A] {

  implicit def N: PartialNext[A]

  def nextOrderWeak(a: A): IsEq[Boolean] =
    N.partialNext(a).map(E.gt(_, a)).getOrElse(true) <-> true

  def nextOrderStrong(a: A, b: A): IsEq[Option[Boolean]] =
    if (E.lt(a, b)) {
      N.partialNext(a).map(E.lteqv(_, b)) <-> Option(true)
    } else {
      Option(true) <-> Option(true)
    }

}

trait PartialNextBoundedLaws[A] extends PartialNextLaws[A] with UpperBoundedLaws[A] {

  def minBoundTerminal: IsEq[Option[A]] =
    N.partialNext(UB.maxBound) <-> None

}

trait PartialPreviousNextLaws[A] extends PartialNextLaws[A] with PartialPreviousLaws[A] with OrderLaws[A] {

  def partialLeftIdentity(a: A): IsEq[Option[A]] =
    P.partialPrevious(a)
      .map(N.partialNext(_) <-> Some(a))
      .getOrElse(Option.empty <-> Option.empty)

  def partialRightIdentity(a: A): IsEq[Option[A]] =
    N.partialNext(a)
      .map(P.partialPrevious(_) <-> Some(a))
      .getOrElse(Option.empty <-> Option.empty)

}

trait PartialPreviousBoundedLaws[A] extends PartialPreviousLaws[A] with LowerBoundedLaws[A] {

  def maxBoundTerminal: IsEq[Option[A]] =
    P.partialPrevious(LB.minBound) <-> None

}

trait BoundedEnumLaws[A] extends PartialPreviousNextLaws[A] with PartialPreviousBoundedLaws[A] with PartialNextBoundedLaws[A] {
}

object BoundedEnumLaws {
  def apply[A](implicit ev: BoundedEnum[A]): BoundedEnumLaws[A] =
    new BoundedEnumLaws[A] {
      val LB = ev
      val E = ev.order
      val UB = ev
      val N = ev
      val P = ev
    }
}
