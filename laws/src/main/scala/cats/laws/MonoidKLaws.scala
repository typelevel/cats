package cats
package laws

/**
 * Laws that must be obeyed by any [[cats.MonoidK]].
 */
trait MonoidKLaws[F[_]] extends SemigroupKLaws[F] {
  override implicit def F: MonoidK[F]

  def leftIdentity[A](a: F[A]): (F[A], F[A]) =
    F.combine(F.empty, a) -> a

  def rightIdentity[A](a: F[A]): (F[A], F[A]) =
    F.combine(a, F.empty) -> a
}

object MonoidKLaws {
  def apply[F[_]](implicit ev: MonoidK[F]): MonoidKLaws[F] =
    new MonoidKLaws[F] { def F = ev }
}
