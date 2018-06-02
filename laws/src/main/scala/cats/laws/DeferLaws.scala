package cats
package laws

/**
 * Laws that must be obeyed by any `Defer`.
 */
trait DeferLaws[F[_]] {
  implicit def F: Defer[F]

  def deferIdentity[A](fa: Unit => F[A]): IsEq[F[A]] =
    F.defer(fa(())) <-> fa(())

  def deferDoesNotEvaluate[A](fa: Unit => F[A]): IsEq[Boolean] = {
    var evaluated = false
    val deferUnit = F.defer {
      evaluated = true;
      fa(())
    }
    evaluated <-> false
  }
}

object DeferLaws {
  def apply[F[_]](implicit ev: Defer[F]): DeferLaws[F] =
    new DeferLaws[F] { def F: Defer[F] = ev }
}
