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
package syntax

trait ApplicativeSyntax {
  implicit final def catsSyntaxApplicativeId[A](a: A): ApplicativeIdOps[A] =
    new ApplicativeIdOps[A](a)
  implicit final def catsSyntaxApplicativeByName[F[_], A](fa: => F[A]): ApplicativeByNameOps[F, A] =
    new ApplicativeByNameOps[F, A](() => fa)
  implicit final def catsSyntaxApplicativeByValue[F[_], A](fa: F[A]): ApplicativeByValueOps[F, A] =
    new ApplicativeByValueOps[F, A](fa)
  @deprecated("Use by-value or by-name version", "2.8.0")
  final def catsSyntaxApplicative[F[_], A](fa: F[A]): ApplicativeOps[F, A] =
    new ApplicativeOps[F, A](fa)
}

final class ApplicativeIdOps[A](private val a: A) extends AnyVal {
  def pure[F[_]](implicit F: Applicative[F]): F[A] = F.pure(a)
}

@deprecated("Use by-value or by-name version", "2.8.0")
final class ApplicativeOps[F[_], A](private val fa: F[A]) extends AnyVal {
  def replicateA(n: Int)(implicit F: Applicative[F]): F[List[A]] = F.replicateA(n, fa)
  def unlessA(cond: Boolean)(implicit F: Applicative[F]): F[Unit] = F.unlessA(cond)(fa)
  def whenA(cond: Boolean)(implicit F: Applicative[F]): F[Unit] = F.whenA(cond)(fa)
}

final class ApplicativeByValueOps[F[_], A](private val fa: F[A]) extends AnyVal {
  def replicateA(n: Int)(implicit F: Applicative[F]): F[List[A]] = F.replicateA(n, fa)
}

final class ApplicativeByNameOps[F[_], A](private val fa: () => F[A]) extends AnyVal {
  def unlessA(cond: Boolean)(implicit F: Applicative[F]): F[Unit] = F.unlessA(cond)(fa())
  def whenA(cond: Boolean)(implicit F: Applicative[F]): F[Unit] = F.whenA(cond)(fa())
}
