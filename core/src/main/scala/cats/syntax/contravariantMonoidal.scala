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

import cats.ContravariantMonoidal

trait ContravariantMonoidalSyntax {
  @deprecated("Kept for binary compatibility", "2.10.0")
  final def catsSyntaxContravariantMonoidal[F[_], A](
    fa: F[A],
    F: ContravariantMonoidal[F]
  ): ContravariantMonoidalOps[F, A] =
    new ContravariantMonoidalOps[F, A] {
      type TypeClassType = ContravariantMonoidal[F]

      val self = fa
      val typeClassInstance = F
    }
}
abstract class ContravariantMonoidalOps[F[_], A] extends ContravariantMonoidal.Ops[F, A]
