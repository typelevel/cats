package cats

/** A monad that also allows you to raise and or handle an error value.
  *
  * This type class allows one to abstract over error-handling monads.
  */
trait MonadError[F[_, _], E] extends Monad[F[E, ?]] {
  def raiseError[A](e: E): F[E, A]

  def handleError[A](fea: F[E, A])(f: E => F[E, A]): F[E, A]
}

object MonadError {
  def apply[F[_, _], E](implicit F: MonadError[F, E]): MonadError[F, E] = F
}
