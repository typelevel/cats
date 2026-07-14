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
package data

import cats.kernel.LowerBounded
import cats.kernel.UpperBounded

object InverseImpl extends InverseInstances with Newtype {
  private[data] def unwrap[A](value: Type[A]): A =
    value.asInstanceOf[A]

  private[data] def create[A](value: A): Type[A] =
    value.asInstanceOf[Type[A]]

  def apply[A](value: A): Inverse[A] =
    create(value)

  implicit def catsInverseOps[A](value: Inverse[A]): InverseOps[A] =
    new InverseOps[A](value)
}

sealed class InverseOps[A](val inverse: Inverse[A]) {
  def value: A = InverseImpl.unwrap(inverse)
}

sealed abstract private[data] class InverseInstances extends InverseInstances0 {
  implicit def hashAndOrderForInverse[A: Hash: Order]: Hash[Inverse[A]] with Order[Inverse[A]] =
    new Hash[Inverse[A]] with Order[Inverse[A]] {
      override def hash(x: Inverse[A]): Int =
        Hash[A].hash(x.value)

      override def compare(x: Inverse[A], y: Inverse[A]): Int =
        Order.reverse[A](Order[A]).compare(x.value, y.value)
    }

  implicit def orderingForInverseFromHashAndOrder[A](implicit A: Order[Inverse[A]]): Ordering[Inverse[A]] =
    A.toOrdering

  implicit def lowerBoundForInverse[A](implicit
    A: UpperBounded[A],
    B: PartialOrder[Inverse[A]]
  ): LowerBounded[Inverse[A]] =
    new LowerBounded[Inverse[A]] {
      override def partialOrder: PartialOrder[Inverse[A]] = B

      override def minBound: Inverse[A] =
        Inverse(A.maxBound)
    }

  implicit def upperBoundForInverse[A](implicit
    A: LowerBounded[A],
    B: PartialOrder[Inverse[A]]
  ): UpperBounded[Inverse[A]] =
    new UpperBounded[Inverse[A]] {
      override def partialOrder: PartialOrder[Inverse[A]] = B

      override def maxBound: Inverse[A] =
        Inverse(A.minBound)
    }
}

sealed private[data] trait InverseInstances0 extends InverseInstances1 {
  implicit def hashAndPartialOrderForInverse[A: Hash: PartialOrder]: Hash[Inverse[A]] with PartialOrder[Inverse[A]] =
    new Hash[Inverse[A]] with PartialOrder[Inverse[A]] {
      override def hash(x: Inverse[A]): Int =
        Hash[A].hash(x.value)

      override def partialCompare(x: Inverse[A], y: Inverse[A]): Double =
        PartialOrder.reverse(PartialOrder[A]).partialCompare(x.value, y.value)
    }
}

sealed private[data] trait InverseInstances1 extends InverseInstances2 {
  implicit def orderForInverse[A: Order]: Order[Inverse[A]] =
    new Order[Inverse[A]] {
      override def compare(x: Inverse[A], y: Inverse[A]): Int =
        Order.reverse(Order[A]).compare(x.value, y.value)
    }
}

sealed private[data] trait InverseInstances2 extends InverseInstances3 {
  implicit def partialOrderForInverse[A: PartialOrder]: PartialOrder[Inverse[A]] =
    new PartialOrder[Inverse[A]] {
      override def partialCompare(x: Inverse[A], y: Inverse[A]): Double =
        PartialOrder.reverse(PartialOrder[A]).partialCompare(x.value, y.value)
    }
}

sealed private[data] trait InverseInstances3 extends InverseInstances4 {
  implicit def hashForInverse[A: Hash]: Hash[Inverse[A]] =
    new Hash[Inverse[A]] {
      override def hash(x: Inverse[A]): Int =
        Hash[A].hash(x.value)

      override def eqv(x: Inverse[A], y: Inverse[A]): Boolean =
        Hash[A].eqv(x.value, y.value)
    }
}

sealed private[data] trait InverseInstances4 {
  implicit def eqForInverse[A: Eq]: Eq[Inverse[A]] =
    Eq.by[Inverse[A], A](_.value)
}
