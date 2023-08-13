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
  @deprecated("Kept for binary compatibility", "2.10.0")
  final def catsSyntaxApply[F[_], A](fa: F[A], F: Apply[F]): Apply.Ops[F, A] =
    new Apply.Ops[F, A] {
      type TypeClassType = Apply[F]

      val self = fa
      val typeClassInstance = F
    }

  implicit final def catsSyntaxApplyFABOps[F[_], A, B](fab: F[A => B]): ApplyFABOps[F, A, B] =
    new ApplyFABOps[F, A, B](fab)

  implicit final def catsSyntaxApplyFABCOps[F[_], A, B, C](ff: F[(A, B) => C]): ApplyFABCOps[F, A, B, C] =
    new ApplyFABCOps[F, A, B, C](ff)

  implicit final def catsSyntaxApplyOps[F[_], A](fa: F[A]): ApplyOps[F, A] =
    new ApplyOps(fa)
}

private[syntax] trait ApplySyntaxBinCompat0 {
  implicit final def catsSyntaxIfApplyOps[F[_]](fa: F[Boolean]): IfApplyOps[F] =
    new IfApplyOps[F](fa)
}

final class ApplyFABOps[F[_], A, B](private val fab: F[A => B]) extends AnyVal {

  /**
   * @see [[Apply.ap]].
   * 
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> val someF: Option[Int => Long] = Some(_.toLong + 1L)
   * scala> val noneF: Option[Int => Long] = None
   * scala> val someInt: Option[Int] = Some(3)
   * scala> val noneInt: Option[Int] = None
   *
   * scala> someF.ap(someInt)
   * res0: Option[Long] = Some(4)
   *
   * scala> noneF.ap(someInt)
   * res1: Option[Long] = None
   *
   * scala> someF.ap(noneInt)
   * res2: Option[Long] = None
   *
   * scala> noneF.ap(noneInt)
   * res3: Option[Long] = None
   * }}}
   */
  def ap(fa: F[A])(implicit F: Apply[F]): F[B] = F.ap(fab)(fa)

  /**
   * Alias for [[ap]].
   */
  def <*>(fa: F[A])(implicit F: Apply[F]): F[B] = F.<*>[A, B](fab)(fa)
}

final class ApplyFABCOps[F[_], A, B, C](private val ff: F[(A, B) => C]) extends AnyVal {

  /**
   * @see [[Apply.ap2]].
   * 
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> val someF: Option[(Int, Int) => Long] = Some((a, b) => (a + b).toLong)
   * scala> val noneF: Option[(Int, Int) => Long] = None
   * scala> val someInt1: Option[Int] = Some(3)
   * scala> val someInt2: Option[Int] = Some(2)
   * scala> val noneInt: Option[Int] = None
   *
   * scala> someF.ap2(someInt1, someInt2)
   * res0: Option[Long] = Some(5)
   *
   * scala> noneF.ap2(someInt1, someInt2)
   * res1: Option[Long] = None
   *
   * scala> someF.ap2(noneInt, noneInt)
   * res2: Option[Long] = None
   *
   * scala> noneF.ap2(noneInt, noneInt)
   * res3: Option[Long] = None
   * }}}
   * 
   */
  def ap2(fa: F[A], fb: F[B])(implicit F: Apply[F]): F[C] = F.ap2(ff)(fa, fb)
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

  /**
   * @see [[Apply.productR]].
   * 
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.Validated
   * scala> import Validated.{Valid, Invalid}
   *
   * scala> type ErrOr[A] = Validated[String, A]
   *
   * scala> val validInt: ErrOr[Int] = Valid(3)
   * scala> val validBool: ErrOr[Boolean] = Valid(true)
   * scala> val invalidInt: ErrOr[Int] = Invalid("Invalid int.")
   * scala> val invalidBool: ErrOr[Boolean] = Invalid("Invalid boolean.")
   *
   * scala> validInt.productR(validBool)
   * res0: ErrOr[Boolean] = Valid(true)
   *
   * scala> invalidInt.productR(validBool)
   * res1: ErrOr[Boolean] = Invalid(Invalid int.)
   *
   * scala> validInt.productR(invalidBool)
   * res2: ErrOr[Boolean] = Invalid(Invalid boolean.)
   *
   * scala> invalidInt.productR(invalidBool)
   * res3: ErrOr[Boolean] = Invalid(Invalid int.Invalid boolean.)
   * }}}    
   */
  def productR[B](fb: F[B])(implicit F: Apply[F]): F[B] = F.productR(fa)(fb)

  /**
   * @see [[Apply.productL]].
   * 
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.Validated
   * scala> import Validated.{Valid, Invalid}
   *
   * scala> type ErrOr[A] = Validated[String, A]
   *
   * scala> val validInt: ErrOr[Int] = Valid(3)
   * scala> val validBool: ErrOr[Boolean] = Valid(true)
   * scala> val invalidInt: ErrOr[Int] = Invalid("Invalid int.")
   * scala> val invalidBool: ErrOr[Boolean] = Invalid("Invalid boolean.")
   *
   * scala> validInt.productL(validBool)
   * res0: ErrOr[Int] = Valid(3)
   *
   * scala> invalidInt.productL(validBool)
   * res1: ErrOr[Int] = Invalid(Invalid int.)
   *
   * scala> validInt.productL(invalidBool)
   * res2: ErrOr[Int] = Invalid(Invalid boolean.)
   *
   * scala> invalidInt.productL(invalidBool)
   * res3: ErrOr[Int] = Invalid(Invalid int.Invalid boolean.)
   * }}}    
   */
  def productL[B](fb: F[B])(implicit F: Apply[F]): F[A] = F.productL(fa)(fb)

  /**
   * Alias for [[productR]].
   */
  def *>[B](fb: F[B])(implicit F: Apply[F]): F[B] = F.*>(fa)(fb)

  /**
   * Alias for [[productL]].
   */
  def <*[B](fb: F[B])(implicit F: Apply[F]): F[A] = F.<*(fa)(fb)

  /**
   * @see [[Apply.map2]].
   * 
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> val someInt: Option[Int] = Some(3)
   * scala> val noneInt: Option[Int] = None
   * scala> val someLong: Option[Long] = Some(4L)
   * scala> val noneLong: Option[Long] = None
   *
   * scala> someInt.map2(someLong)((i, l) => i.toString + l.toString)
   * res0: Option[String] = Some(34)
   *
   * scala> someInt.map2(noneLong)((i, l) => i.toString + l.toString)
   * res0: Option[String] = None
   *
   * scala> noneInt.map2(noneLong)((i, l) => i.toString + l.toString)
   * res0: Option[String] = None
   *
   * scala> noneInt.map2(someLong)((i, l) => i.toString + l.toString)
   * res0: Option[String] = None
   * }}} 
   */
  def map2[B, C](fb: F[B])(f: (A, B) => C)(implicit F: Apply[F]): F[C] =
    F.map2(fa, fb)(f)

  /**
   * @see [[Apply.map2Eval]].
   * 
   * Example:
   * {{{
   * scala> import cats.{Eval, Later}
   * scala> import cats.syntax.all._
   * 
   * scala> val bomb: Eval[Option[Int]] = Later(sys.error("boom"))
   * scala> val x: Option[Int] = None
   * 
   * scala> x.map2Eval(bomb)(_ + _).value
   * res0: Option[Int] = None
   * }}} 
   */
  def map2Eval[B, C](fb: Eval[F[B]])(f: (A, B) => C)(implicit F: Apply[F]): Eval[F[C]] =
    F.map2Eval(fa, fb)(f)
}
