package cats
package laws

/**
 * Laws that must be obeyed by any [[cats.SemigroupK]].
 */
trait SemigroupKLaws[F[_]] {
  implicit def F: SemigroupK[F]

  def semigroupKAssociative[A](a: F[A], b: F[A], c: F[A]): IsEq[F[A]] =
   F.combine(F.combine(a, b), c) <-> F.combine(a, F.combine(b, c))
}

object SemigroupKLaws {
  def apply[F[_]](implicit ev: SemigroupK[F]): SemigroupKLaws[F] =
    new SemigroupKLaws[F] { def F: SemigroupK[F] = ev }
}
