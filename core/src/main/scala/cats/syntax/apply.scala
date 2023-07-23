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
  @deprecated("Use `ApplySyntaxBinCompat1`", "2.10.0")
  final def catsSyntaxApply[F[_], A](fa: F[A], F: Apply[F]): Apply.Ops[F, A] =
    new Apply.Ops[F, A] {
      type TypeClassType = Apply[F]

      val self = fa
      val typeClassInstance = F
    }

  implicit final def catsSyntaxApplyOps[F[_], A](fa: F[A]): ApplyOps[F, A] =
    new ApplyOps(fa)
}

private[syntax] trait ApplySyntaxBinCompat0 {
  implicit final def catsSyntaxIfApplyOps[F[_]](fa: F[Boolean]): IfApplyOps[F] =
    new IfApplyOps[F](fa)
}

private[syntax] trait ApplySyntaxBinCompat1 {
  implicit final def applyFABOps[F[_], A, B](fab: F[A => B]): ApplyFABOps[F, A, B] =
    new ApplyFABOps[F, A, B](fab)

  implicit final def apply2Ops[F[_], A, B, C](ff: F[(A, B) => C]): Apply2Ops[F, A, B, C] =
    new Apply2Ops[F, A, B, C](ff)

  implicit final def productOps[F[_], A, B](fa: F[A]): ProductOps[F, A, B] =
    new ProductOps[F, A, B](fa)

  implicit final def map2Ops[F[_], A, B, C](fa: F[A]): Map2Ops[F, A, B, C] =
    new Map2Ops[F, A, B, C](fa)
}

final class ApplyFABOps[F[_], A, B](private val fab: F[A => B]) extends AnyVal {
  def ap(fa: F[A])(implicit F: Apply[F]): F[B] = F.ap(fab)(fa)

  def <*>(fa: F[A])(implicit F: Apply[F]): F[B] = F.<*>[A, B](fab)(fa)
}

final class Apply2Ops[F[_], A, B, C](private val ff: F[(A, B) => C]) extends AnyVal {
  def ap2(fa: F[A], fb: F[B])(implicit F: Apply[F]): F[C] = F.ap2(ff)(fa, fb)
}

final class ProductOps[F[_], A, B](private val fa: F[A]) extends AnyVal {
  def productR(fb: F[B])(implicit F: Apply[F]): F[B] = F.productR(fa)(fb)

  def productL(fb: F[B])(implicit F: Apply[F]): F[A] = F.productL(fa)(fb)

  def *>(fb: F[B])(implicit F: Apply[F]): F[B] = F.*>(fa)(fb)

  def <*(fb: F[B])(implicit F: Apply[F]): F[A] = F.<*(fa)(fb)
}

final class Map2Ops[F[_], A, B, C](private val fa: F[A]) extends AnyVal {
  def map2(fb: F[B])(f: (A, B) => C)(implicit F: Apply[F]): F[C] =
    F.map2(fa, fb)(f)

  def map2Eval(fb: Eval[F[B]])(f: (A, B) => C)(implicit F: Apply[F]): Eval[F[C]] =
    F.map2Eval(fa, fb)(f)
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
