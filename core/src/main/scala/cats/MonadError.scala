package cats

/** A monad that also allows you to raise and or handle an error value.
  *
  * This type class allows one to abstract over error-handling monads.
  */
trait MonadError[F[_], E] extends Monad[F] {
  def raiseError[A](e: E): F[A]

  def handleError[A](fea: F[A])(f: E => F[A]): F[A]
}

object MonadError {
  def apply[F[_], E](implicit F: MonadError[F, E]): MonadError[F, E] = F
}
