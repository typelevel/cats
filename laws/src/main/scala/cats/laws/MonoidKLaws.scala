package cats
package laws

/**
 * Laws that must be obeyed by any `cats.MonoidK`.
 */
trait MonoidKLaws[F[_]] extends SemigroupKLaws[F] {
  override implicit def F0: MonoidK[F]

  def monoidKLeftIdentity[A](a: F[A]): IsEq[F[A]] =
    F0.combineK(F0.empty, a) <-> a

  def monoidKRightIdentity[A](a: F[A]): IsEq[F[A]] =
    F0.combineK(a, F0.empty) <-> a
}

object MonoidKLaws {
  def apply[F[_]](implicit ev: MonoidK[F]): MonoidKLaws[F] =
    new MonoidKLaws[F] { def F0: MonoidK[F] = ev }
}
