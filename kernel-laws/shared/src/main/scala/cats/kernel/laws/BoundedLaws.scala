package cats.kernel.laws

import cats.kernel.{LowerBounded, PartialOrder, UpperBounded}

trait LowerBoundedLaws[A] extends PartialOrderLaws[A] {
  implicit def B: LowerBounded[A]

  def boundLteqv(x: A): IsEq[Boolean] =
    E.lteqv(B.minBound, x) <-> true
}

object LowerBoundedLaws {
  def apply[A](implicit ev: LowerBounded[A]): LowerBoundedLaws[A] =
    new LowerBoundedLaws[A] {
      def B: LowerBounded[A] = ev
      def E: PartialOrder[A] = ev.partialOrder
    }
}

trait UpperBoundedLaws[A] extends PartialOrderLaws[A] {
  implicit def B: UpperBounded[A]

  def boundGteqv(x: A): IsEq[Boolean] =
    E.gteqv(B.maxBound, x) <-> true
}

object UpperBoundedLaws {
  def apply[A](implicit ev: UpperBounded[A]): UpperBoundedLaws[A] =
    new UpperBoundedLaws[A] {
      def B: UpperBounded[A] = ev
      def E: PartialOrder[A] = ev.partialOrder
    }
}
