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
   * Sequences the specified finalizer ensuring evaluation regardless of
   * whether or not the target `F[A]` raises an error.
   *
   * If `raiseError` is analogous to `throw` and `handleErrorWith` is analogous to
   * `catch`, then `guarantee` is analogous to `finally`.  JVM exception semantics
   * are mirrored with respect to error raised within the finalizer (i.e. errors
   * raised within the finalizer will shade any errors raised by the primary action).
   *
   * @see [[raiseError]]
   * @see [[handleErrorWith]]
   */
  def guarantee[A](fa: F[A], finalizer: F[Unit]): F[A] = {
    flatMap(attempt(fa)) { e =>
      flatMap(finalizer)(_ => e.fold(raiseError, pure))
    }
  }
}

object MonadError {
  def apply[F[_], E](implicit F: MonadError[F, E]): MonadError[F, E] = F
}
