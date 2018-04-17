package cats
package laws

/**
  * Laws that must be obeyed by any `cats.ErrorControl`.
  */
trait ErrorControlLaws[F[_], G[_], E] {
  implicit def M: ErrorControl[F, G, E]

  def deriveHandleError[A](fa: F[A])
                       (f: E => A): IsEq[F[A]] =
    M.accept(M.intercept(fa)(f)) <-> M.monadErrorF.handleError(fa)(f)

  def deriveAttempt[A](fa: F[A]): IsEq[F[Either[E, A]]]=
    M.accept(M.trial(fa)) <-> M.monadErrorF.attempt(fa)

  def deriveEnsureOr[A](ga: G[A])
                    (error: A => E)
                    (predicate: A => Boolean): IsEq[F[A]] =
    M.monadErrorF.ensureOr(M.accept(ga))(error)(predicate) <-> M.assureOr(ga)(error)(predicate)

  def gNeverHasErrors[A](ga: G[A], f: E => A): IsEq[G[A]] =
    M.intercept(M.accept(ga))(f) <-> ga

  def raiseErrorHandleBlunderWith[A](e: E, f: E => G[A]): IsEq[G[A]] =
    M.controlError(M.monadErrorF.raiseError[A](e))(f) <-> f(e)

  def handleErrorPureIsPure[A](a: A, f: E => G[A]): IsEq[G[A]] =
    M.controlError(M.monadErrorF.pure(a))(f) <-> M.applicativeG.pure(a)

  def raiseErrorHandleBlunder[A](e: E, f: E => A): IsEq[G[A]] =
    M.intercept(M.monadErrorF.raiseError[A](e))(f) <-> M.applicativeG.pure(f(e))

  def raiseErrorEndeavor[A](e: E): IsEq[G[Either[E, A]]] =
    M.trial(M.monadErrorF.raiseError[A](e)) <-> M.applicativeG.pure(Left(e))

  def endeavorAbsolve[A](fa: F[A]): IsEq[F[A]] =
    M.absolve(M.trial(fa)) <-> fa
}