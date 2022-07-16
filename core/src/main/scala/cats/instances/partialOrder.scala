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
package instances
import cats.kernel.instances.unit._

trait PartialOrderInstances extends kernel.instances.PartialOrderInstances {
  implicit val catsContravariantMonoidalForPartialOrder: ContravariantMonoidal[PartialOrder] =
    new ContravariantMonoidal[PartialOrder] {

      /**
       * Derive a `PartialOrder` for `B` given a `PartialOrder[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: PartialOrder[A])(f: B => A): PartialOrder[B] = PartialOrder.by[B, A](f)(fa)

      def product[A, B](fa: PartialOrder[A], fb: PartialOrder[B]): PartialOrder[(A, B)] =
        new PartialOrder[(A, B)] {
          def partialCompare(x: (A, B), y: (A, B)): Double = {
            val z = fa.partialCompare(x._1, y._1)
            if (z == 0.0) fb.partialCompare(x._2, y._2) else z
          }
        }

      def unit: PartialOrder[Unit] = Order[Unit]
    }
}
