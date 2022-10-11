/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats
package laws

import cats.data.EitherT

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

  def raiseErrorAttempt(e: E): IsEq[F[Either[E, Unit]]] =
    F.attempt(F.raiseError[Unit](e)) <-> F.pure(Left(e))

  def pureAttempt[A](a: A): IsEq[F[Either[E, A]]] =
    F.attempt(F.pure(a)) <-> F.pure(Right(a))

  def handleErrorWithConsistentWithRecoverWith[A](fa: F[A], f: E => F[A]): IsEq[F[A]] =
    F.handleErrorWith(fa)(f) <-> F.recoverWith(fa) { case x => f(x) }

  def handleErrorConsistentWithRecover[A](fa: F[A], f: E => A): IsEq[F[A]] =
    F.handleError(fa)(f) <-> F.recover(fa) { case x => f(x) }

  def recoverConsistentWithRecoverWith[A](fa: F[A], pf: PartialFunction[E, A]): IsEq[F[A]] =
    F.recover(fa)(pf) <-> F.recoverWith(fa)(pf.andThen(F.pure(_)))

  def attemptConsistentWithAttemptT[A](fa: F[A]): IsEq[EitherT[F, E, A]] =
    EitherT(F.attempt(fa)) <-> F.attemptT(fa)

  def attemptFromEitherConsistentWithPure[A](eab: Either[E, A]): IsEq[F[Either[E, A]]] =
    F.attempt(F.fromEither(eab)) <-> F.pure(eab)

  def onErrorPure[A](a: A, f: E => F[Unit]): IsEq[F[A]] =
    F.onError(F.pure(a)) { case x => f(x) } <-> F.pure(a)

  def onErrorRaise[A](fa: F[A], e: E, fb: F[Unit]): IsEq[F[A]] =
    F.onError(F.raiseError[A](e)) { case _ => fb } <-> F.map2(fb, F.raiseError[A](e))((_, b) => b)

  def adaptErrorPure[A](a: A, f: E => E): IsEq[F[A]] =
    F.adaptError(F.pure(a)) { case x => f(x) } <-> F.pure(a)

  def adaptErrorRaise[A](e: E, f: E => E): IsEq[F[A]] =
    F.adaptError(F.raiseError[A](e)) { case x => f(x) } <-> F.raiseError(f(e))

  def redeemDerivedFromAttemptMap[A, B](fa: F[A], fe: E => B, fs: A => B): IsEq[F[B]] =
    F.redeem(fa)(fe, fs) <-> F.map(F.attempt(fa))(_.fold(fe, fs))

  /*
   * These laws, taken together with applicativeErrorHandle, show that errors dominate in
   * ap, *and* show that handle has lexical semantics over ap. F.unit is used in both laws
   * because we don't have another way of expressing "an F[_] which does *not* contain any
   * errors". We could make these laws considerably stronger if such a thing were
   * expressible. Specifically, what we're missing here is the ability to say that
   * raiseError distributes over an *arbitrary* number of aps.
   */

  def raiseErrorDistributesOverApLeft[A](h: E => F[A], e: E) =
    F.handleErrorWith(F.ap(F.raiseError[Unit => A](e))(F.unit))(h) <-> h(e)

  def raiseErrorDistributesOverApRight[A](h: E => F[A], e: E) =
    F.handleErrorWith(F.ap(F.pure((a: A) => a))(F.raiseError[A](e)))(h) <-> h(e)
}

object ApplicativeErrorLaws {
  def apply[F[_], E](implicit ev: ApplicativeError[F, E]): ApplicativeErrorLaws[F, E] =
    new ApplicativeErrorLaws[F, E] { def F: ApplicativeError[F, E] = ev }
}
