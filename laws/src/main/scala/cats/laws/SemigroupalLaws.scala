package cats
package laws

/**
 * Laws that must be obeyed by any `cats.Semigroupal`.
 */
trait SemigroupalLaws[F[_]] {
  implicit def F: Semigroupal[F]

  def semigroupalAssociativity[A, B, C](fa: F[A], fb: F[B], fc: F[C]): (F[(A, (B, C))], F[((A, B), C)]) =
    (F.product(fa, F.product(fb, fc)), F.product(F.product(fa, fb), fc))
}

object SemigroupalLaws {
  def apply[F[_]](implicit ev: Semigroupal[F]): SemigroupalLaws[F] =
    new SemigroupalLaws[F] { val F = ev }
}
