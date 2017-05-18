package cats

/**
 * A monad that also allows you to raise and or handle an error value.
 *
 * This type class allows one to abstract over error-handling monads.
 */
trait MonadError[F[_], E] extends ApplicativeError[F, E] with Monad[F] {

  /**
   * Turns a successful value into an error if it does not satisfy a given predicate.
   */
  def ensure[A](fa: F[A])(error: => E)(predicate: A => Boolean): F[A] =
    flatMap(fa)(a => if (predicate(a)) pure(a) else raiseError(error))

  /**
    * Turns a successful value into an error specified by the `error` function if it does not satisfy a given predicate.
    */
  def ensureOr[A](fa: F[A])(error: A => E)(predicate: A => Boolean): F[A] =
    flatMap(fa)(a => if (predicate(a)) pure(a) else raiseError(error(a)))

}

object MonadError {
  def apply[F[_], E](implicit F: MonadError[F, E]): MonadError[F, E] = F
}
