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

package cats.kernel

/**
 * Lifting of the [[Eq]] class to binary type constructors.
 *
 * @see [[https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Functor-Classes.html#t:Eq2]]
 */
trait Eq2[F[_, _]] extends Any with Serializable {

  /**
   * Lift equality tests through the type constructor.
   */
  def liftEq2[A, B, C, D](compareAB: (A, B) => Boolean, compareCD: (C, D) => Boolean, x: F[A, C], y: F[B, D]): Boolean

  // derived //

  /**
   * Returns `true` if `x` and `y` are equivalent, `false` otherwise.
   */
  def eqv2[A, B](x: F[A, B], y: F[A, B])(implicit A: Eq[A], B: Eq[B]): Boolean =
    liftEq2[A, A, B, B](A.eqv, B.eqv, x, y)

  /**
   * Returns `false` if `x` and `y` are equivalent, `true` otherwise.
   */
  def neqv2[A: Eq, B: Eq](x: F[A, B], y: F[A, B]): Boolean =
    !eqv2[A, B](x, y)
}

object Eq2 extends Eq2Instances0 {
  @inline def apply[F[_, _]](implicit ev: Eq2[F]): Eq2[F] = ev
}

private[kernel] trait Eq2Instances0 {
  implicit def catsKernelEq2ForEither: Eq2[Either] =
    cats.kernel.instances.either.catsStdOrder2AndHash2ForEither
}
