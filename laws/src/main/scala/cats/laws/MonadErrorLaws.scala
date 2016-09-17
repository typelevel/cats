package cats
package laws

// Taken from http://functorial.com/psc-pages/docs/Control/Monad/Error/Class/index.html
trait MonadErrorLaws[F[_], E] extends ApplicativeErrorLaws[F, E] with MonadLaws[F] {
  implicit override def FE: MonadError[F, E]
  implicit override def F: Monad[F] = FE.monad

  def monadErrorLeftZero[A, B](e: E, f: A => F[B]): IsEq[F[B]] =
    F.flatMap(FE.raiseError[A](e))(f) <-> FE.raiseError[B](e)
}

object MonadErrorLaws {
  def apply[F[_], E](implicit ev: MonadError[F, E]): MonadErrorLaws[F, E] =
    new MonadErrorLaws[F, E] { def FE: MonadError[F, E] = ev }
}
