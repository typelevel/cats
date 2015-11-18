package cats
package laws

trait MonoidalLaws[F[_]] {

  implicit def F: Monoidal[F]

  def associativity[A, B, C](fa: F[A], fb: F[B], fc: F[C]): (F[(A, (B, C))], F[((A, B), C)]) =
    (F.product(fa, F.product(fb, fc)), F.product(F.product(fa, fb), fc))

  def leftIdentity[A](funit: F[Unit], fa: F[A]): (F[(Unit, A)], F[A]) =
    (F.product(funit, fa), fa)

  def rightIdentity[A](fa: F[A], funit: F[Unit]): (F[(A, Unit)], F[A]) =
    (F.product(fa, funit), fa)

}

object MonoidalLaws {

  def apply[F[_]](implicit ev: Monoidal[F]): MonoidalLaws[F] =
    new MonoidalLaws[F] { val F = ev }

}