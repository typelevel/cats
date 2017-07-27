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

  def adaptErrorPure[A](a: A, f: E => E): IsEq[F[A]] =
    F.adaptError(F.pure(a))(PartialFunction(f)) <-> F.pure(a)

  def adaptErrorRaise[A](e: E, f: E => E): IsEq[F[A]] =
    F.adaptError(F.raiseError[A](e))(PartialFunction(f)) <-> F.raiseError(f(e))
}

object MonadErrorLaws {
  def apply[F[_], E](implicit ev: MonadError[F, E]): MonadErrorLaws[F, E] =
    new MonadErrorLaws[F, E] { def F: MonadError[F, E] = ev }
}
