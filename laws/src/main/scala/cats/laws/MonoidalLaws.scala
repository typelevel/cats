package cats
package laws

trait MonoidalLaws[F[_]] {

  implicit def F: Monoidal[F]

  def monoidalAssociativity[A, B, C](fa: F[A], fb: F[B], fc: F[C]): (F[(A, (B, C))], F[((A, B), C)]) =
    (F.product(fa, F.product(fb, fc)), F.product(F.product(fa, fb), fc))

}

object MonoidalLaws {

  def apply[F[_]](implicit ev: Monoidal[F]): MonoidalLaws[F] =
    new MonoidalLaws[F] { val F = ev }

}