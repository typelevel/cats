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

import java.lang.Double.isNaN

/**
 * Lifting of the [[Order]] class to unary type constructors.
 *
 * @note The default implementation of the [[PartialOrder1]] provided in this
 *       class is lossy in terms of magnitude. That is, if the underlying
 *       comparison returns `-100.123`, it will be converted into `-1.0`. If
 *       for some reason you desire the magnitude to be preserved for the
 *       [[PartialOrder1]] instance, then you should override this function.
 *
 * @see [[https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Functor-Classes.html#t:Ord1]]
 */
trait Order1[F[_]] extends Any with PartialOrder1[F] {

  /**
   * Lift ordering tests through the type constructor.
   */
  def liftCompare[A, B](compare: (A, B) => Int, x: F[A], y: F[B]): Int

  // derived //

  override def liftPartialCompare[A, B](partialCompare: (A, B) => Double, x: F[A], y: F[B]): Double = {
    // This default implementation has to work around some representation
    // issues with Double/Int. See the writeup in Order2.

    val nanInt: Int = Int.MinValue

    def partialToTotal(value: Double): Int =
      if (isNaN(value)) {
        nanInt
      } else if (value < 0d) {
        -1
      } else if (value > 0d) {
        1
      } else {
        0
      }

    def totalToPartial(value: Int): Double =
      if (value == nanInt) {
        Double.NaN
      } else {
        value.toDouble
      }

    totalToPartial(
      liftCompare(
        (a: A, b: B) => partialToTotal(partialCompare(a, b)),
        x,
        y
      )
    )
  }
}

object Order1 extends Order1Instances0 {
  @inline def apply[F[_]](implicit ev: Order1[F]): Order1[F] = ev
}

private[kernel] trait Order1Instances0 extends Order1LowPriorityInstances0 {

  /**
   * The instance for `cats.Id`.
   *
   * @note `cats.Id` is defined in the `core` package, but this definition is
   *       needed to allow for the automatic derivation of types such as
   *       `Order[Id[F[A]]]`, when we have a `Order1[F]` and a
   *       `Order[A]`. Lacking this definition, the scala compiler will not be
   *       able to derive the `Order[Id[F[A]]]` instance.
   */
  implicit val catsKernelOrder1InstanceForId: Order1[({ type Id[α] = α })#Id] =
    new Order1[({ type Id[α] = α })#Id] {
      override def liftCompare[A, B](compare: (A, B) => Int, x: A, y: B): Int =
        compare(x, y)
    }
}

private[kernel] trait Order1LowPriorityInstances0 {

  /**
   * Derive an [[Order1]] instance from an [[Order2]] instance if we have an
   * [[Order]] instance for the second type paremeter.
   */
  implicit def order2ToOrder1L[F[_, _], A](implicit F: Order2[F], A: Order[A]): Order1[({ type L[α] = F[α, A] })#L] =
    new Order1[({ type L[α] = F[α, A] })#L] {
      override def liftCompare[B, C](compare: (B, C) => Int, x: F[B, A], y: F[C, A]): Int =
        F.liftCompare2(compare, A.compare, x, y)
    }

  /**
   * Derive an [[Order1]] instance from an [[Order2]] instance if we have an
   * [[Order]] instance for the first type paremeter.
   */
  implicit def order2ToOrder1R[F[_, _], A](implicit F: Order2[F], A: Order[A]): Order1[({ type L[α] = F[A, α] })#L] =
    new Order1[({ type L[α] = F[A, α] })#L] {
      override def liftCompare[B, C](compare: (B, C) => Int, x: F[A, B], y: F[A, C]): Int =
        F.liftCompare2(A.compare, compare, x, y)
    }
}
