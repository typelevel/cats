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
 * Lifting of the [[PartialOrder]] class to unary type constructors.
 */
trait PartialOrder1[F[_]] extends Any with Eq1[F] {

  /**
   * Lift partial ordering tests through the type constructor.
   */
  def liftPartialCompare[A, B](partialCompare: (A, B) => Double, x: F[A], y: F[B]): Double

  // final //

  // from Eq1

  // In order to implement liftEq in terms of liftPartialCompare, we need to
  // choose Double values to represent true and false. We choose 0 to
  // represent true, to match with the canonical definition of PartialOrder
  // equality, and Double.NaN to match false.
  override def liftEq[A, B](compare: (A, B) => Boolean, x: F[A], y: F[B]): Boolean =
    liftPartialCompare[A, B](
      (a, b) => if (compare(a, b)) 0d else Double.NaN,
      x,
      y
    ) == 0d

  /**
   * Result of comparing `x` with `y`. Returns NaN if operands are not
   * comparable. If operands are comparable, returns a Double whose
   * sign is:
   *
   *   - negative iff `x < y`
   *   - zero     iff `x = y`
   *   - positive iff `x > y`
   */
  def partialCompare1[A](x: F[A], y: F[A])(implicit A: PartialOrder[A]): Double =
    liftPartialCompare[A, A](A.partialCompare, x, y)
}

object PartialOrder1 extends PartialOrder1Instances0 {
  @inline def apply[F[_]](implicit ev: PartialOrder1[F]): PartialOrder1[F] =
    ev
}

private[kernel] trait PartialOrder1Instances0 extends PartialOrder1LowPriorityInstances0 {

  /** @see [[Order1#catsKernelOrder1InstanceForId]] */
  implicit def catsKernelPartialOrder1InstanceForId: PartialOrder1[({ type Id[α] = α })#Id] =
    Order1.catsKernelOrder1InstanceForId
}

private[kernel] trait PartialOrder1LowPriorityInstances0 {

  /**
   * Derive a [[PartialOrder1]] instance from a [[PartialOrder2]] instance if
   * we have a [[PartialOrder]] instance for the second type paremeter.
   */
  implicit def partialOrder2ToPartialOrder1L[F[_, _], A](implicit
    F: PartialOrder2[F],
    A: PartialOrder[A]
  ): PartialOrder1[({ type L[α] = F[α, A] })#L] =
    new PartialOrder1[({ type L[α] = F[α, A] })#L] {
      override def liftPartialCompare[B, C](partialCompare: (B, C) => Double, x: F[B, A], y: F[C, A]): Double =
        F.liftPartialCompare2(partialCompare, A.partialCompare, x, y)
    }

  /**
   * Derive a [[PartialOrder1]] instance from a [[PartialOrder2]] instance if
   * we have a [[PartialOrder]] instance for the first type paremeter.
   */
  implicit def partialOrder2ToPartialOrder1R[F[_, _], A](implicit
    F: PartialOrder2[F],
    A: PartialOrder[A]
  ): PartialOrder1[({ type L[α] = F[A, α] })#L] =
    new PartialOrder1[({ type L[α] = F[A, α] })#L] {
      override def liftPartialCompare[B, C](partialCompare: (B, C) => Double, x: F[A, B], y: F[A, C]): Double =
        F.liftPartialCompare2(A.partialCompare, partialCompare, x, y)
    }
}
