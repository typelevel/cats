package cats.kernel.laws

import cats.kernel.{LowerBounded, UpperBounded}

trait LowerBoundedLaws[A] extends PartialOrderLaws[A] {
  implicit override def E: LowerBounded[A]

  def boundLteqv(x: A): IsEq[Boolean] =
    E.lteqv(E.minBound, x) <-> true
}

object LowerBoundedLaws {
  def apply[A](implicit ev: LowerBounded[A]): LowerBoundedLaws[A] =
    new LowerBoundedLaws[A] { def E: LowerBounded[A] = ev }
}

trait UpperBoundedLaws[A] extends PartialOrderLaws[A] {
  implicit override def E: UpperBounded[A]

  def boundGteqv(x: A): IsEq[Boolean] =
    E.gteqv(E.maxBound, x) <-> true
}

object UpperBoundedLaws {
  def apply[A](implicit ev: UpperBounded[A]): UpperBoundedLaws[A] =
    new UpperBoundedLaws[A] { def E: UpperBounded[A] = ev }
}
