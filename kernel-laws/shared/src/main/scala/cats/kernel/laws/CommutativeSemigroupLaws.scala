package cats.kernel.laws

import cats.kernel.CommutativeSemigroup

trait CommutativeSemigroupLaws[A] extends SemigroupLaws[A] {
  implicit override def S: CommutativeSemigroup[A]

  def commutative(x: A, y: A): IsEq[A] =
    S.combine(x, y) <-> S.combine(y, x)

}

object CommutativeSemigroupLaws {
  def apply[A](implicit ev: CommutativeSemigroup[A]): CommutativeSemigroupLaws[A] =
    new CommutativeSemigroupLaws[A] { def S: CommutativeSemigroup[A] = ev }
}
