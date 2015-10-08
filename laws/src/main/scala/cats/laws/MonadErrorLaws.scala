package cats
package laws

import cats.data.{Xor, XorT}

// Taken from http://functorial.com/psc-pages/docs/Control/Monad/Error/Class/index.html
trait MonadErrorLaws[F[_], E] extends MonadLaws[F] {
  implicit override def F: MonadError[F, E]

  def monadErrorLeftZero[A, B](e: E, f: A => F[B]): IsEq[F[B]] =
    F.flatMap(F.raiseError[A](e))(f) <-> F.raiseError[B](e)

  def monadErrorHandleWith[A](e: E, f: E => F[A]): IsEq[F[A]] =
    F.handleErrorWith(F.raiseError[A](e))(f) <-> f(e)

  def monadErrorHandle[A](e: E, f: E => A): IsEq[F[A]] =
    F.handleError(F.raiseError[A](e))(f) <-> F.pure(f(e))

  def handleErrorWithPure[A](a: A, f: E => F[A]): IsEq[F[A]] =
    F.handleErrorWith(F.pure(a))(f) <-> F.pure(a)

  def handleErrorPure[A](a: A, f: E => A): IsEq[F[A]] =
    F.handleError(F.pure(a))(f) <-> F.pure(a)

  def raiseErrorAttempt(e: E): IsEq[F[E Xor Unit]] =
    F.attempt(F.raiseError[Unit](e)) <-> F.pure(Xor.left(e))

  def pureAttempt[A](a: A): IsEq[F[E Xor A]] =
    F.attempt(F.pure(a)) <-> F.pure(Xor.right(a))

  def handleErrorWithConsistentWithRecoverWith[A](fa: F[A], f: E => F[A]): IsEq[F[A]] =
    F.handleErrorWith(fa)(f) <-> F.recoverWith(fa)(PartialFunction(f))

  def handleErrorConsistentWithRecover[A](fa: F[A], f: E => A): IsEq[F[A]] =
    F.handleError(fa)(f) <-> F.recover(fa)(PartialFunction(f))

  def recoverConsistentWithRecoverWith[A](fa: F[A], pf: PartialFunction[E, A]): IsEq[F[A]] =
    F.recover(fa)(pf) <-> F.recoverWith(fa)(pf andThen F.pure)

  def attemptConsistentWithAttemptT[A](fa: F[A]): IsEq[XorT[F, E, A]] =
    XorT(F.attempt(fa)) <-> F.attemptT(fa)
}

object MonadErrorLaws {
  def apply[F[_], E](implicit ev: MonadError[F, E]): MonadErrorLaws[F, E] =
    new MonadErrorLaws[F, E] { def F: MonadError[F, E] = ev }
}
