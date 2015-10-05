package cats

import cats.data.{Xor, XorT}

/** A monad that also allows you to raise and or handle an error value.
  *
  * This type class allows one to abstract over error-handling monads.
  */
trait MonadError[F[_], E] extends Monad[F] {
  def raiseError[A](e: E): F[A]

  def handleError[A](fa: F[A])(f: E => F[A]): F[A]

  def attempt[A](fa: F[A]): F[E Xor A] = handleError(
    map(fa)(Xor.right[E, A])
  )(e => pure(Xor.left(e)))

  def attemptT[A](fa: F[A]): XorT[F, E, A] = XorT(attempt(fa))

  def recover[A](fa: F[A])(pf: PartialFunction[E, A]): F[A] =
    handleError(fa)(e =>
      (pf andThen pure) applyOrElse(e, raiseError))

  def recoverWith[A](fa: F[A])(pf: PartialFunction[E, F[A]]): F[A] =
    handleError(fa)(e =>
      pf applyOrElse(e, raiseError))
}

object MonadError {
  def apply[F[_], E](implicit F: MonadError[F, E]): MonadError[F, E] = F
}
