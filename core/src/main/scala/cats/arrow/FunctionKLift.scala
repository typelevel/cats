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
package arrow

private[arrow] trait FunctionKLift {
  protected type τ[F[_], G[_]]

  /**
   * Lifts function `f` of `F[A] => G[A]` into a `FunctionK[F, G]`.
   *
   * {{{
   *   def headOption[A](list: List[A]): Option[A] = list.headOption
   *   val lifted = FunctionK.liftFunction[List, Option](headOption)
   * }}}
   *
   * Note: The weird `τ[F, G]` parameter is there to compensate for
   * the lack of polymorphic function types in Scala 2.
   * 
   * It is present in the Scala 3 API to simplify cross-compilation.
   */
  def liftFunction[F[_], G[_]](f: F[τ[F, G]] => G[τ[F, G]]): FunctionK[F, G] =
    new FunctionK[F, G] {
      def apply[A](fa: F[A]): G[A] = f.asInstanceOf[F[A] => G[A]](fa)
    }
}
