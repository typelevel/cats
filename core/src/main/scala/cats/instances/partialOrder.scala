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

import scala.annotation.tailrec

trait PartialOrderInstances extends kernel.instances.PartialOrderInstances {
  implicit val catsContravariantMonoidalForPartialOrder: ContravariantMonoidal[PartialOrder] =
    new ContravariantMonoidal[PartialOrder] {

      /**
       * Derive a `PartialOrder` for `B` given a `PartialOrder[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: PartialOrder[A])(f: B => A): PartialOrder[B] = PartialOrder.by[B, A](f)(fa)

      def product[A, B](fa: PartialOrder[A], fb: PartialOrder[B]): PartialOrder[(A, B)] = { (x, y) =>
        val z = fa.partialCompare(x._1, y._1)
        if (z == 0.0) fb.partialCompare(x._2, y._2) else z
      }

      def unit: PartialOrder[Unit] = Order[Unit]
    }

  implicit def catsDeferForPartialOrder: Defer[PartialOrder] = PartialOrderInstances.catsDeferForPartialOrderCache
}
object PartialOrderInstances {
  private val catsDeferForPartialOrderCache: Defer[PartialOrder] =
    new Defer[PartialOrder] {
      case class Deferred[A](fa: () => PartialOrder[A]) extends PartialOrder[A] {
        private lazy val resolved: PartialOrder[A] = {
          @tailrec
          def loop(f: () => PartialOrder[A]): PartialOrder[A] =
            f() match {
              case Deferred(f) => loop(f)
              case next        => next
            }

          loop(fa)
        }
        override def partialCompare(x: A, y: A): Double = resolved.partialCompare(x, y)
      }

      override def defer[A](fa: => PartialOrder[A]): PartialOrder[A] = {
        lazy val cachedFa = fa
        Deferred(() => cachedFa)
      }
    }
}
