package cats
package laws

/**
 * Laws that must be obeyed by any [[cats.MonoidK]].
 */
trait MonoidKLaws[F[_]] extends SemigroupKLaws[F] {
  override implicit def F: MonoidK[F]

  def monoidKLeftIdentity[A](a: F[A]): IsEq[F[A]] =
    F.combine(F.empty, a) <-> a

  def monoidKRightIdentity[A](a: F[A]): IsEq[F[A]] =
    F.combine(a, F.empty) <-> a
}

object MonoidKLaws {
  def apply[F[_]](implicit ev: MonoidK[F]): MonoidKLaws[F] =
    new MonoidKLaws[F] { def F: MonoidK[F] = ev }
}
