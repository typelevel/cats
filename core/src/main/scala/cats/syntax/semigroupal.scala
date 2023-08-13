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

trait SemigroupalSyntax {
  @deprecated("Use `catsSyntaxSemigroupalOps2`", "2.10.0")
  final def catsSyntaxSemigroupal[F[_], A](fa: F[A], F: Semigroupal[F]): SemigroupalOps[F, A] =
    new SemigroupalOps[F, A] {
      type TypeClassType = Semigroupal[F]

      val self = fa
      val typeClassInstance = F
    }

  implicit def catsSyntaxSemigroupalOps2[F[_], A](fa: F[A]): SemigroupalOps2[F, A] =
    new SemigroupalOps2[F, A](fa)

}

final class SemigroupalOps2[F[_], A](private val fa: F[A]) extends AnyVal {

  /**
   * @see [[Semigroupal.product]]
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> val noneInt: Option[Int] = None
   * scala> val some3: Option[Int] = Some(3)
   * scala> val noneString: Option[String] = None
   * scala> val someFoo: Option[String] = Some("foo")
   *
   * scala> noneInt.product(noneString)
   * res0: Option[(Int, String)] = None
   *
   * scala> noneInt.product(someFoo)
   * res1: Option[(Int, String)] = None
   *
   * scala> some3.product(noneString)
   * res2: Option[(Int, String)] = None
   *
   * scala> some3.product(someFoo)
   * res3: Option[(Int, String)] = Some((3,foo))
   * }}}
   */
  def product[B](fb: F[B])(implicit F: Semigroupal[F]): F[(A, B)] = F.product(fa, fb)

  @deprecated("Replaced by an apply syntax, e.g. instead of (a |@| b).map(...) use (a, b).mapN(...)", "1.0.0-MF")
  def |@|[B](fb: F[B]): SemigroupalBuilder[F]#SemigroupalBuilder2[A, B] =
    new SemigroupalBuilder[F] |@| fa |@| fb
}

abstract class SemigroupalOps[F[_], A] extends Semigroupal.Ops[F, A] {

  @deprecated("Replaced by an apply syntax, e.g. instead of (a |@| b).map(...) use (a, b).mapN(...)", "1.0.0-MF")
  final private[syntax] def |@|[B](fb: F[B]): SemigroupalBuilder[F]#SemigroupalBuilder2[A, B] =
    new SemigroupalBuilder[F] |@| self |@| fb

}
