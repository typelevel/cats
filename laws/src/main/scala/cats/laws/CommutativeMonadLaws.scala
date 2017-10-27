package cats
package laws

/**
 * Laws that must be obeyed by any `CommutativeMonad`.
 */
trait CommutativeMonadLaws[F[_]] extends MonadLaws[F] with CommutativeFlatMapLaws[F] with CommutativeApplicativeLaws[F] {
  implicit override def F: CommutativeMonad[F]
}

object CommutativeMonadLaws {
  def apply[F[_]](implicit ev: CommutativeMonad[F]): CommutativeMonadLaws[F] =
    new CommutativeMonadLaws[F] { def F: CommutativeMonad[F] = ev }
}
