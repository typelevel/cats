package cats.kernel.laws

import cats.kernel.Semigroup

trait SemigroupLaws[A] {
  implicit def S: Semigroup[A]

  def semigroupAssociative(x: A, y: A, z: A): IsEq[A] =
    S.combine(S.combine(x, y), z) <-> S.combine(x, S.combine(y, z))

  def repeat1(a: A): IsEq[A] =
    S.combineN(a, 1) <-> a

  def repeat2(a: A): IsEq[A] =
    S.combineN(a, 2) <-> S.combine(a, a)

  def combineAllOption(xs: Vector[A]): IsEq[Option[A]] =
    S.combineAllOption(xs) <-> xs.reduceOption(S.combine)

}

object SemigroupLaws {
  def apply[A](implicit ev: Semigroup[A]): SemigroupLaws[A] =
    new SemigroupLaws[A] { def S: Semigroup[A] = ev }
}
