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
 * Lifting of the [[PartialOrder]] class to binary type constructors.
 */
trait PartialOrder2[F[_, _]] extends Any with Eq2[F] {

  /**
   * Lift partial ordering tests through the type constructor.
   */
  def liftPartialCompare2[A, B, C, D](partialCompareAB: (A, B) => Double,
                                      partialCompareCD: (C, D) => Double,
                                      x: F[A, C],
                                      y: F[B, D]
  ): Double

  // derived //

  // from Eq2

  // In order to implement liftEq2 in terms of liftPartialCompare2, we need to
  // choose Double values to represent true and false. We choose 0 to
  // represent true, to match with the canonical definition of PartialOrder
  // equality, and Double.NaN to match false.
  override def liftEq2[A, B, C, D](compareAB: (A, B) => Boolean,
                                   compareCD: (C, D) => Boolean,
                                   x: F[A, C],
                                   y: F[B, D]
  ): Boolean =
    liftPartialCompare2[A, B, C, D](
      (a, b) => if (compareAB(a, b)) 0d else Double.NaN,
      (c, d) => if (compareCD(c, d)) 0d else Double.NaN,
      x,
      y
    ) == 0d

  // other //

  /**
   * Result of comparing `x` with `y`. Returns NaN if operands are not
   * comparable. If operands are comparable, returns a Double whose
   * sign is:
   *
   *   - negative iff `x < y`
   *   - zero     iff `x = y`
   *   - positive iff `x > y`
   */
  def partialCompare2[A, B](x: F[A, B], y: F[A, B])(implicit A: PartialOrder[A], B: PartialOrder[B]): Double =
    liftPartialCompare2[A, A, B, B](A.partialCompare, B.partialCompare, x, y)
}

object PartialOrder2 extends PartialOrder2Instances0 {
  @inline def apply[F[_, _]](implicit ev: PartialOrder2[F]): PartialOrder2[F] =
    ev
}

private[kernel] trait PartialOrder2Instances0 {
  implicit def catsKernelPartialOrder2ForEither: PartialOrder2[Either] =
    cats.kernel.instances.either.catsStdOrder2AndHash2ForEither
}
