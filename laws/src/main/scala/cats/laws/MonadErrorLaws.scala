package cats
package laws

// Taken from http://functorial.com/psc-pages/docs/Control/Monad/Error/Class/index.html
trait MonadErrorLaws[F[_, _], E] extends MonadLaws[F[E, ?]] {
  implicit override def F: MonadError[F, E]

  def monadErrorLeftZero[A, B](e: E, f: A => F[E, B]): IsEq[F[E, B]] =
    F.flatMap(F.raiseError[A](e))(f) <-> F.raiseError[B](e)

  def monadErrorHandle[A](e: E, f: E => F[E, A]): IsEq[F[E, A]] =
    F.handleError(F.raiseError[A](e))(f) <-> f(e)

  def monadErrorPure[A](a: A, f: E => F[E, A]): IsEq[F[E, A]] =
    F.handleError(F.pure(a))(f) <-> F.pure(a)
}

object MonadErrorLaws {
  def apply[F[_, _], E](implicit ev: MonadError[F, E]): MonadErrorLaws[F, E] =
    new MonadErrorLaws[F, E] { def F: MonadError[F, E] = ev }
}
