package cats
package laws

/**
 * Laws that must be obeyed by any [[cats.Monoid]].
 */
trait MonoidLaws[A] extends SemigroupLaws[A] {
  override implicit def A: Monoid[A]

  def leftIdentity(a: A): (A, A) =
    A.combine(A.empty, a) -> a

  def rightIdentity(a: A): (A, A) =
    A.combine(a, A.empty) -> a
}

object MonoidLaws {
  def apply[A](implicit ev: Monoid[A]): MonoidLaws[A] =
    new MonoidLaws[A] { def A = ev }
}
