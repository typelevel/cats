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

trait AlternativeSyntax {

  implicit final def catsSyntaxUnite[F[_], G[_], A](fga: F[G[A]]): UniteOps[F, G, A] =
    new UniteOps[F, G, A](fga)

  implicit final def catsSyntaxAlternativeSeparate[F[_], G[_, _], A, B](fgab: F[G[A, B]]): SeparateOps[F, G, A, B] =
    new SeparateOps[F, G, A, B](fgab)

  implicit final def catsSyntaxAlternativeGuard(b: Boolean): GuardOps =
    new GuardOps(b)
}

final class UniteOps[F[_], G[_], A](protected val fga: F[G[A]]) extends AnyVal with UniteOpsBinCompat0[F, G, A] {

  @deprecated("use a FlatMap-constrained version instead", "2.6.2")
  protected def unite(F: Monad[F], A: Alternative[F], G: Foldable[G]): F[A] =
    A.unite(fga)(F, G)
}

sealed private[syntax] trait UniteOpsBinCompat0[F[_], G[_], A] extends Any { self: UniteOps[F, G, A] =>

  /**
   * See [[[Alternative.unite[G[_],A](fga:F[G[A]])(implicitFM:cats\.FlatMap[F]*]]]
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> val x: List[Vector[Int]] = List(Vector(1, 2), Vector(3, 4))
   * scala> x.unite
   * res0: List[Int] = List(1, 2, 3, 4)
   * }}}
   */
  def unite(implicit F: FlatMap[F], A: Alternative[F], G: Foldable[G]): F[A] =
    A.unite[G, A](fga)
}

final class SeparateOps[F[_], G[_, _], A, B](protected val fgab: F[G[A, B]])
    extends AnyVal
    with SeparateOpsBinCompat0[F, G, A, B] {

  @deprecated("use a FlatMap-constrained version instead", "2.6.2")
  protected def separate(F: Monad[F], A: Alternative[F], G: Bifoldable[G]): (F[A], F[B]) =
    A.separate[G, A, B](fgab)(F, G)

  /**
   * See [[Alternative.separateFoldable]]
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> val l: List[Either[String, Int]] = List(Right(1), Left("error"))
   * scala> l.separateFoldable
   * res0: (List[String], List[Int]) = (List(error),List(1))
   * }}}
   */
  def separateFoldable(implicit F: Foldable[F], A: Alternative[F], G: Bifoldable[G]): (F[A], F[B]) =
    A.separateFoldable[G, A, B](fgab)
}

sealed private[syntax] trait SeparateOpsBinCompat0[F[_], G[_, _], A, B] extends Any { self: SeparateOps[F, G, A, B] =>

  /**
   * See [[[Alternative.separate[G[_,_],A,B](fgab:F[G[A,B]])(implicitFM:cats\.FlatMap[F]* Alternative.separate]]]
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> val l: List[Either[String, Int]] = List(Right(1), Left("error"))
   * scala> l.separate
   * res0: (List[String], List[Int]) = (List(error),List(1))
   * }}}
   */
  def separate(implicit F: FlatMap[F], A: Alternative[F], G: Bifoldable[G]): (F[A], F[B]) =
    A.separate[G, A, B](fgab)
}

final class GuardOps(private val condition: Boolean) extends AnyVal {

  /**
   * See [[Alternative.guard]]
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> def even(i: Int): Option[String] = (i % 2 == 0).guard[Option].as("even")
   * scala> even(2)
   * res0: Option[String] = Some(even)
   * scala> even(3)
   * res1: Option[String] = None
   * }}}
   */
  def guard[F[_]](implicit F: Alternative[F]): F[Unit] = F.guard(condition)
}
