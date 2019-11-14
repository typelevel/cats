package cats
package laws

// Taken from http://functorial.com/psc-pages/docs/Control/Monad/Error/Class/index.html
trait MonadErrorLaws[F[_], E] extends ApplicativeErrorLaws[F, E] with MonadLaws[F] {
  implicit override def F: MonadError[F, E]

  def monadErrorLeftZero[A, B](e: E, f: A => F[B]): IsEq[F[B]] =
    F.flatMap(F.raiseError[A](e))(f) <-> F.raiseError[B](e)

  def monadErrorEnsureConsistency[A](fa: F[A], e: E, p: A => Boolean): IsEq[F[A]] =
    F.ensure(fa)(e)(p) <-> F.flatMap(fa)(a => if (p(a)) F.pure(a) else F.raiseError(e))

  def monadErrorEnsureOrConsistency[A](fa: F[A], e: A => E, p: A => Boolean): IsEq[F[A]] =
    F.ensureOr(fa)(e)(p) <-> F.flatMap(fa)(a => if (p(a)) F.pure(a) else F.raiseError(e(a)))

  def rethrowAttempt[A](fa: F[A]): IsEq[F[A]] =
    F.rethrow(F.attempt(fa)) <-> fa

  def redeemWithDerivedFromAttemptFlatMap[A, B](fa: F[A], fe: E => F[B], fs: A => F[B]): IsEq[F[B]] =
    F.redeemWith(fa)(fe, fs) <-> F.flatMap(F.attempt(fa))(_.fold(fe, fs))
}

object MonadErrorLaws {
  def apply[F[_], E](implicit ev: MonadError[F, E]): MonadErrorLaws[F, E] =
    new MonadErrorLaws[F, E] { def F: MonadError[F, E] = ev }
}
