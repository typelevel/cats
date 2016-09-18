package cats
package laws

/**
 * Laws that must be obeyed by any `cats.SemigroupK`.
 */
trait SemigroupKLaws[F[_]] {
  implicit def F0: SemigroupK[F]

  def semigroupKAssociative[A](a: F[A], b: F[A], c: F[A]): IsEq[F[A]] =
    F0.combineK(F0.combineK(a, b), c) <-> F0.combineK(a, F0.combineK(b, c))
}

object SemigroupKLaws {
  def apply[F[_]](implicit ev: SemigroupK[F]): SemigroupKLaws[F] =
    new SemigroupKLaws[F] { def F0: SemigroupK[F] = ev }
}
