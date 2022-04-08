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
package conversions
import cats.arrow.Profunctor

trait VarianceConversions extends VarianceConversionsLowPriority {
  implicit def autoWidenBifunctor[F[_, _]: Bifunctor, A, B >: A, C, D >: C](fac: F[A, C]): F[B, D] =
    Bifunctor[F].leftWiden(Bifunctor[F].rightFunctor.widen(fac))

  implicit def autoConvertProfunctorVariance[F[_, _]: Profunctor, A, B <: A, C, D >: C](fac: F[A, C]): F[B, D] =
    Profunctor[F].leftNarrow(Profunctor[F].rightWiden(fac))

  implicit def autoNarrowContravariant[F[_]: Contravariant, A, B <: A](fa: F[A]): F[B] = Contravariant[F].narrow(fa)

}

private[cats] trait VarianceConversionsLowPriority {
  implicit def autoWidenFunctor[F[_]: Functor, A, B >: A](fa: F[A]): F[B] = Functor[F].widen(fa)
}
