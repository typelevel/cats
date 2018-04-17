package cats
package laws

/**
  * Laws that must be obeyed by any `cats.ErrorControl`.
  */
trait ErrorControlLaws[F[_], G[_], E] {
  implicit def E: ErrorControl[F, G, E]

  def deriveHandleError[A](fa: F[A])
                       (f: E => A): IsEq[F[A]] =
    E.accept(E.intercept(fa)(f)) <-> E.monadErrorF.handleError(fa)(f)

  def deriveAttempt[A](fa: F[A]): IsEq[F[Either[E, A]]]=
    E.accept(E.trial(fa)) <-> E.monadErrorF.attempt(fa)

  def deriveEnsureOr[A](ga: G[A])
                    (error: A => E)
                    (predicate: A => Boolean): IsEq[F[A]] =
    E.monadErrorF.ensureOr(E.accept(ga))(error)(predicate) <-> E.assureOr(ga)(error)(predicate)

  def gNeverHasErrors[A](ga: G[A], f: E => A): IsEq[G[A]] =
    E.intercept(E.accept(ga))(f) <-> ga

  def raiseErrorControlError[A](e: E, f: E => G[A]): IsEq[G[A]] =
    E.controlError(E.monadErrorF.raiseError[A](e))(f) <-> f(e)

  def controlErrorPureIsPure[A](a: A, f: E => G[A]): IsEq[G[A]] =
    E.controlError(E.monadErrorF.pure(a))(f) <-> E.applicativeG.pure(a)

  def raiseErrorIntercept[A](e: E, f: E => A): IsEq[G[A]] =
    E.intercept(E.monadErrorF.raiseError[A](e))(f) <-> E.applicativeG.pure(f(e))

  def raiseErrorTrial[A](e: E): IsEq[G[Either[E, A]]] =
    E.trial(E.monadErrorF.raiseError[A](e)) <-> E.applicativeG.pure(Left(e))

  def trialAbsolve[A](fa: F[A]): IsEq[F[A]] =
    E.absolve(E.trial(fa)) <-> fa
}

object ErrorControlLaws {
  def apply[F[_], G[_], E](implicit ev: ErrorControl[F, G, E]): ErrorControlLaws[F, G, E] =
    new ErrorControlLaws[F, G, E] { def E: ErrorControl[F, G, E] = ev }
}
