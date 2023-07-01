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

trait FlatMapSyntax extends FlatMap.ToFlatMapOps {

  implicit final def catsSyntaxFlatten[F[_]: FlatMap, A](ffa: F[F[A]]): FlattenOps[F, A] =
    new FlattenOps[F, A](ffa)

  implicit final def catsSyntaxIfM[F[_]: FlatMap](fa: F[Boolean]): IfMOps[F] =
    new IfMOps[F](fa)

  implicit final def catsSyntaxFlatMapIdOps[A](a: A): FlatMapIdOps[A] =
    new FlatMapIdOps[A](a)

  implicit final def catsSyntaxFlatMapOps[F[_]: FlatMap, A](fa: F[A]): FlatMapOps[F, A] =
    new FlatMapOps[F, A](fa)
}

final class FlatMapOps[F[_], A](private val fa: F[A]) extends AnyVal {

  /**
   * Alias for [[flatMap]].
   */
  def >>=[B](f: A => F[B])(implicit F: FlatMap[F]): F[B] = F.flatMap(fa)(f)

  /**
   * Alias for `fa.flatMap(_ => fb)`.
   *
   * Unlike `*>`, `fb` is defined as a by-name parameter, allowing this
   * method to be used in cases where computing `fb` is not stack safe
   * unless suspended in a `flatMap`.
   */
  def >>[B](fb: => F[B])(implicit F: FlatMap[F]): F[B] = F.flatMap(fa)(_ => fb)

  @deprecated("Use <* instead", "1.0.0-RC1")
  private[syntax] def <<[B](fb: F[B])(implicit F: FlatMap[F]): F[A] = F.productL(fa)(fb)
  @deprecated("Use productREval instead.", "1.0.0-RC2")
  private[syntax] def followedByEval[B](fb: Eval[F[B]])(implicit F: FlatMap[F]): F[B] =
    F.productREval(fa)(fb)

  @deprecated("Use productLEval instead.", "1.0.0-RC2")
  private[syntax] def forEffectEval[B](fb: Eval[F[B]])(implicit F: FlatMap[F]): F[A] =
    F.productLEval(fa)(fb)

  /**
   * Like an infinite loop of >> calls. This is most useful effect loops
   * that you want to run forever in for instance a server.
   *
   * This will be an infinite loop, or it will return an F[Nothing].
   *
   * Be careful using this.
   * For instance, a List of length k will produce a list of length k^n at iteration
   * n. This means if k = 0, we return an empty list, if k = 1, we loop forever
   * allocating single element lists, but if we have a k > 1, we will allocate
   * exponentially increasing memory and very quickly OOM.
   */
  def foreverM[B](implicit F: FlatMap[F]): F[B] = F.foreverM[A, B](fa)
}

final class FlattenOps[F[_], A](private val ffa: F[F[A]]) extends AnyVal {

  /**
   * Flatten nested `F` values.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> type ErrorOr[A] = Either[String, A]
   * scala> val x: ErrorOr[ErrorOr[Int]] = 3.asRight.asRight
   * scala> x.flatten
   * res0: ErrorOr[Int] = Right(3)
   * }}}
   */
  def flatten(implicit F: FlatMap[F]): F[A] = F.flatten(ffa)
}

final class IfMOps[F[_]](private val fa: F[Boolean]) extends AnyVal {

  /**
   * A conditional lifted into the `F` context.
   *
   * Example:
   * {{{
   * scala> import cats.{Eval, Now}
   * scala> import cats.syntax.all._
   *
   * scala> val b1: Eval[Boolean] = Now(true)
   * scala> val asInt1: Eval[Int] = b1.ifM(Now(1), Now(0))
   * scala> asInt1.value
   * res0: Int = 1
   *
   * scala> val b2: Eval[Boolean] = Now(false)
   * scala> val asInt2: Eval[Int] = b2.ifM(Now(1), Now(0))
   * scala> asInt2.value
   * res1: Int = 0
   * }}}
   */
  def ifM[B](ifTrue: => F[B], ifFalse: => F[B])(implicit F: FlatMap[F]): F[B] = F.ifM(fa)(ifTrue, ifFalse)
}

final class FlatMapIdOps[A](private val a: A) extends AnyVal {

  /**
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> val a: Int = 10
   * scala> a.tailRecM[Option,String](i => if (i == 20) Some(Right("done")) else Some(Left(i+1)))
   * res0: Option[String] = Some(done)
   *
   * }}}
   */
  def tailRecM[F[_], B](f: A => F[Either[A, B]])(implicit F: FlatMap[F]): F[B] = F.tailRecM(a)(f)

  /**
   * iterateForeverM is almost exclusively useful for effect types. For instance,
   * A may be some state, we may take the current state, run some effect to get
   * a new state and repeat.
   */
  def iterateForeverM[F[_], B](f: A => F[A])(implicit F: FlatMap[F]): F[B] = F.iterateForeverM[A, B](a)(f)
}

trait FlatMapOptionSyntax {
  implicit final def catsSyntaxFlatMapOptionOps[F[_]: FlatMap, A](foa: F[Option[A]]): FlatMapOptionOps[F, A] =
    new FlatMapOptionOps[F, A](foa)
}

final class FlatMapOptionOps[F[_], A](private val fopta: F[Option[A]]) extends AnyVal {

  /**
   * This repeats an F until we get defined values. This can be useful
   * for polling type operations on State (or RNG) Monads, or in effect
   * monads.
   */
  def untilDefinedM(implicit F: FlatMap[F]): F[A] = F.untilDefinedM[A](fopta)
}
