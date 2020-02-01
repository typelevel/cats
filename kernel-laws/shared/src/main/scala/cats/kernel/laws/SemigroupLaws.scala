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

  def reverseReverses(a: A, b: A): IsEq[A] =
    S.combine(a, b) <-> S.reverse.combine(b, a)

  def reverseRepeat1(a: A): IsEq[A] = {
    val rev = S.reverse
    rev.combineN(a, 1) <-> a
  }

  def reverseRepeat2(a: A): IsEq[A] = {
    val rev = S.reverse
    rev.combineN(a, 2) <-> rev.combine(a, a)
  }

  def reverseCombineAllOption(xs: Vector[A]): IsEq[Option[A]] = {
    val rev = S.reverse
    rev.combineAllOption(xs) <-> xs.reduceOption(rev.combine)
  }

  def intercalateIntercalates(a: A, m: A, b: A): IsEq[A] =
    S.combine(a, S.combine(m, b)) <-> S.intercalate(m).combine(a, b)

  def intercalateRepeat1(m: A, a: A): IsEq[A] = {
    val withMiddle = S.intercalate(m)
    withMiddle.combineN(a, 1) <-> a
  }

  def intercalateRepeat2(m: A, a: A): IsEq[A] = {
    val withMiddle = S.intercalate(m)
    withMiddle.combineN(a, 2) <-> withMiddle.combine(a, a)
  }

  def intercalateCombineAllOption(m: A, xs: Vector[A]): IsEq[Option[A]] = {
    val withMiddle = S.intercalate(m)
    withMiddle.combineAllOption(xs) <-> xs.reduceOption(withMiddle.combine)
  }
}

object SemigroupLaws {
  def apply[A](implicit ev: Semigroup[A]): SemigroupLaws[A] =
    new SemigroupLaws[A] { def S: Semigroup[A] = ev }
}
