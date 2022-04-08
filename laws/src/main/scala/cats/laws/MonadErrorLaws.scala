/*
 * Copyright (c) 2022 Typelevel
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

  // See https://github.com/typelevel/cats/pull/3203 for an explanation of why these two overrides
  // are needed in 2.x for binary compatibility.
  override def adaptErrorPure[A](a: A, f: E => E): IsEq[F[A]] =
    F.adaptError(F.pure(a)) { case x => f(x) } <-> F.pure(a)

  override def adaptErrorRaise[A](e: E, f: E => E): IsEq[F[A]] =
    F.adaptError(F.raiseError[A](e)) { case x => f(x) } <-> F.raiseError(f(e))
}

object MonadErrorLaws {
  def apply[F[_], E](implicit ev: MonadError[F, E]): MonadErrorLaws[F, E] =
    new MonadErrorLaws[F, E] { def F: MonadError[F, E] = ev }
}
