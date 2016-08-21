package cats
package laws

import cats.data.{Xor, XorT}

// Taken from http://functorial.com/psc-pages/docs/Control/Monad/Error/Class/index.html
trait ApplicativeErrorLaws[F[_], E] extends ApplicativeLaws[F] {
  implicit override def F: ApplicativeError[F, E]

  def applicativeErrorHandleWith[A](e: E, f: E => F[A]): IsEq[F[A]] =
    F.handleErrorWith(F.raiseError[A](e))(f) <-> f(e)

  def applicativeErrorHandle[A](e: E, f: E => A): IsEq[F[A]] =
    F.handleError(F.raiseError[A](e))(f) <-> F.pure(f(e))

  def handleErrorWithPure[A](a: A, f: E => F[A]): IsEq[F[A]] =
    F.handleErrorWith(F.pure(a))(f) <-> F.pure(a)

  def handleErrorPure[A](a: A, f: E => A): IsEq[F[A]] =
    F.handleError(F.pure(a))(f) <-> F.pure(a)

  def raiseErrorAttempt(e: E): IsEq[F[Xor[E, Unit]]] =
    F.attempt(F.raiseError[Unit](e)) <-> F.pure(Xor.Left(e))

  def pureAttempt[A](a: A): IsEq[F[Xor[E, A]]] =
    F.attempt(F.pure(a)) <-> F.pure(Xor.Right(a))

  def handleErrorWithConsistentWithRecoverWith[A](fa: F[A], f: E => F[A]): IsEq[F[A]] =
    F.handleErrorWith(fa)(f) <-> F.recoverWith(fa)(PartialFunction(f))

  def handleErrorConsistentWithRecover[A](fa: F[A], f: E => A): IsEq[F[A]] =
    F.handleError(fa)(f) <-> F.recover(fa)(PartialFunction(f))

  def recoverConsistentWithRecoverWith[A](fa: F[A], pf: PartialFunction[E, A]): IsEq[F[A]] =
    F.recover(fa)(pf) <-> F.recoverWith(fa)(pf andThen F.pure)

  def attemptConsistentWithAttemptT[A](fa: F[A]): IsEq[XorT[F, E, A]] =
    XorT(F.attempt(fa)) <-> F.attemptT(fa)
}

object ApplicativeErrorLaws {
  def apply[F[_], E](implicit ev: ApplicativeError[F, E]): ApplicativeErrorLaws[F, E] =
    new ApplicativeErrorLaws[F, E] { def F: ApplicativeError[F, E] = ev }
}
