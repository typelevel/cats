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

trait ApplySyntax extends TupleSemigroupalSyntax {
  final def catsSyntaxApply[F[_], A](fa: F[A])(implicit F: Apply[F]): Apply.Ops[F, A] =
    new Apply.Ops[F, A] {
      type TypeClassType = Apply[F]

      val self = fa
      val typeClassInstance = F
    }

  implicit final def catsSyntaxApplyBinCompat1[F[_], A](fa: F[A]): ApplyBinCompat1[F, A] =
    new ApplyBinCompat1(fa)

  implicit final def catsSyntaxApplyOps[F[_], A](fa: F[A]): ApplyOps[F, A] =
    new ApplyOps(fa)
}

private[syntax] trait ApplySyntaxBinCompat0 {
  implicit final def catsSyntaxIfApplyOps[F[_]](fa: F[Boolean]): IfApplyOps[F] =
    new IfApplyOps[F](fa)
}

final class ApplyBinCompat1[F[_], A](private val fa: F[A]) extends AnyVal {
  def ap[B, C](fb: F[B])(implicit ev$1: A <:< (B => C), F: Apply[F]): F[C] =
    F.ap(fa.asInstanceOf[F[B => C]])(fb)
  def productR[B](fb: F[B])(implicit F: Apply[F]): F[B] = F.productR[A, B](fa)(fb)
  def productL[B](fb: F[B])(implicit F: Apply[F]): F[A] = F.productL[A, B](fa)(fb)
  @inline final def <*>[B, C](fa: F[B])(implicit ev$1: A <:< (B => C), F: Apply[F]): F[C] =
    F.<*>[B, C](fa.asInstanceOf[F[B => C]])(fa)
  @inline def *>[B](fb: F[B])(implicit F: Apply[F]): F[B] = F.*>[A, B](fa)(fb)
  @inline def <*[B](fb: F[B])(implicit F: Apply[F]): F[A] = F.<*[A, B](fa)(fb)
  def ap2[B, C, D](fa: F[B], fb: F[C])(implicit ev$1: A <:< ((B, C) => D), F: Apply[F]): F[D] =
    F.ap2[B, C, D](fa.asInstanceOf[F[(B, C) => D]])(fa, fb)
  def map2[B, C](fb: F[B])(f: (A, B) => C)(implicit F: Apply[F]): F[C] = F.map2[A, B, C](fa, fb)(f)
  def map2Eval[B, C](fb: Eval[F[B]])(f: (A, B) => C)(implicit F: Apply[F]): Eval[F[C]] =
    F.map2Eval[A, B, C](fa, fb)(f)
}

final class IfApplyOps[F[_]](private val fcond: F[Boolean]) extends AnyVal {

  @deprecated("Dangerous method, use ifM (a flatMap) or ifF (a map) instead", "2.6.2")
  def ifA[A](ifTrue: F[A], ifFalse: F[A])(implicit F: Apply[F]): F[A] = F.ifA(fcond)(ifTrue, ifFalse)
}

final class ApplyOps[F[_], A](private val fa: F[A]) extends AnyVal {

  /**
   * Alias for [[Apply.productR]].
   */
  @deprecated("Use *> or productR instead.", "1.0.0-RC2")
  @inline private[syntax] def followedBy[B](fb: F[B])(implicit F: Apply[F]): F[B] =
    F.productR(fa)(fb)

  /**
   * Alias for [[Apply.productL]].
   */
  @deprecated("Use <* or productL instead.", "1.0.0-RC2")
  @inline private[syntax] def forEffect[B](fb: F[B])(implicit F: Apply[F]): F[A] =
    F.productL(fa)(fb)
}
