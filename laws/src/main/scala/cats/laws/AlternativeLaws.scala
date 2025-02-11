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

import cats.syntax.all.*
import cats.kernel.compat.scalaVersionSpecific.*

trait AlternativeLaws[F[_]] extends NonEmptyAlternativeLaws[F] with MonoidKLaws[F] {
  implicit override def F: Alternative[F]
  implicit override def algebra[A]: Monoid[F[A]] = F.algebra[A]

  def alternativeRightAbsorption[A, B](ff: F[A => B]): IsEq[F[B]] =
    (ff.ap(F.empty[A])) <-> F.empty[B]

  // Perhaps should be deprecated in favor of nonEmptyAlternativeLeftDistributivity
  def alternativeLeftDistributivity[A, B](fa: F[A], fa2: F[A], f: A => B): IsEq[F[B]] =
    nonEmptyAlternativeLeftDistributivity[A, B](fa, fa2, f)

  // Perhaps should be deprecated in favor of nonEmptyAlternativeRightDistributivity
  def alternativeRightDistributivity[A, B](fa: F[A], ff: F[A => B], fg: F[A => B]): IsEq[F[B]] =
    nonEmptyAlternativeRightDistributivity(fa, ff, fg)

  def fromIterableOnce[A](as: Iterable[A]): IsEq[F[A]] =
    F.fromIterableOnce(as) <-> F.combineAllK(as.iterator.map(F.pure(_)))

}

@suppressUnusedImportWarningForScalaVersionSpecific
object AlternativeLaws {
  def apply[F[_]](implicit ev: Alternative[F]): AlternativeLaws[F] =
    new AlternativeLaws[F] { def F: Alternative[F] = ev }

  def composed[M[_], N[_]](implicit M: Alternative[M], N: Applicative[N]): AlternativeLaws[λ[α => M[N[α]]]] =
    new AlternativeLaws[λ[α => M[N[α]]]] {
      def F: Alternative[λ[α => M[N[α]]]] = M.compose[N]
    }
}
