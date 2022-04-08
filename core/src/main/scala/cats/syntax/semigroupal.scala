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
package syntax

trait SemigroupalSyntax {
  implicit final def catsSyntaxSemigroupal[F[_], A](fa: F[A])(implicit F: Semigroupal[F]): SemigroupalOps[F, A] =
    new SemigroupalOps[F, A] {
      type TypeClassType = Semigroupal[F]

      val self = fa
      val typeClassInstance = F
    }
}

abstract class SemigroupalOps[F[_], A] extends Semigroupal.Ops[F, A] {

  @deprecated("Replaced by an apply syntax, e.g. instead of (a |@| b).map(...) use (a, b).mapN(...)", "1.0.0-MF")
  final private[syntax] def |@|[B](fb: F[B]): SemigroupalBuilder[F]#SemigroupalBuilder2[A, B] =
    new SemigroupalBuilder[F] |@| self |@| fb

}
