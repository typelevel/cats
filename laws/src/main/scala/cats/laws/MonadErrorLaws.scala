package cats
package laws

// Taken from http://functorial.com/psc-pages/docs/Control/Monad/Error/Class/index.html
trait MonadErrorLaws[F[_], E] extends ApplicativeErrorLaws[F, E] with MonadLaws[F] {
  implicit override def F: MonadError[F, E]

  def monadErrorLeftZero[A, B](e: E, f: A => F[B]): IsEq[F[B]] =
    F.flatMap(F.raiseError[A](e))(f) <-> F.raiseError[B](e)
}

object MonadErrorLaws {
  def apply[F[_], E](implicit ev: MonadError[F, E]): MonadErrorLaws[F, E] =
    new MonadErrorLaws[F, E] { def F: MonadError[F, E] = ev }
}
