package cats
package laws

/**
 * Laws that must be obeyed by any `cats.Cartesian`.
 */
trait CartesianLaws[F[_]] {
  implicit def F: Cartesian[F]

  def cartesianAssociativity[A, B, C](fa: F[A], fb: F[B], fc: F[C]): (F[(A, (B, C))], F[((A, B), C)]) =
    (F.product(fa, F.product(fb, fc)), F.product(F.product(fa, fb), fc))
}

object CartesianLaws {
  def apply[F[_]](implicit ev: Cartesian[F]): CartesianLaws[F] =
    new CartesianLaws[F] { val F = ev }
}
