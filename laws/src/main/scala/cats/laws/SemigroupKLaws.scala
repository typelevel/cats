package cats
package laws

/**
 * Laws that must be obeyed by any `cats.SemigroupK`.
 */
trait SemigroupKLaws[F[_]] {
  implicit def F: SemigroupK[F]

  def semigroupKAssociative[A](a: F[A], b: F[A], c: F[A]): IsEq[F[A]] =
    F.combineK(F.combineK(a, b), c) <-> F.combineK(a, F.combineK(b, c))

  def repeat1K[A](a: F[A]): IsEq[F[A]] =
    F.combineNK(a, 1) <-> a

  def repeat2[A](a: F[A]): IsEq[F[A]] =
    F.combineNK(a, 2) <-> F.combineK(a, a)

  def combineAllOptionK[A](xs: Vector[F[A]]): IsEq[Option[F[A]]] =
    F.combineAllOptionK(xs) <-> xs.reduceOption(F.combineK[A])

  def reverseReversesK[A](a: F[A], b: F[A]): IsEq[F[A]] =
    F.combineK(a, b) <-> F.reverse.combineK(b, a)

  def reverseRepeat1K[A](a: F[A]): IsEq[F[A]] = {
    val rev = F.reverse
    rev.combineNK(a, 1) <-> a
  }

  def reverseRepeat2K[A](a: F[A]): IsEq[F[A]] = {
    val rev = F.reverse
    rev.combineNK(a, 2) <-> rev.combineK(a, a)
  }

  def reverseCombineAllOptionK[A](xs: Vector[F[A]]): IsEq[Option[F[A]]] = {
    val rev = F.reverse
    rev.combineAllOptionK(xs) <-> xs.reduceOption(rev.combineK[A])
  }

}

object SemigroupKLaws {
  def apply[F[_]](implicit ev: SemigroupK[F]): SemigroupKLaws[F] =
    new SemigroupKLaws[F] { def F: SemigroupK[F] = ev }
}
