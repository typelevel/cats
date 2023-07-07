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

trait OrderInstances extends kernel.instances.OrderInstances {

  implicit val catsContravariantMonoidalForOrder: ContravariantMonoidal[Order] =
    new ContravariantMonoidal[Order] {

      /**
       * Provides trivial order
       */
      def unit: Order[Unit] = Order[Unit]

      /**
       * Derive an `Order` for `B` given an `Order[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: Order[A])(f: B => A): Order[B] =
        Order.by(f)(fa)

      def product[A, B](fa: Order[A], fb: Order[B]): Order[(A, B)] = { (x, y) =>
        val z = fa.compare(x._1, y._1)
        if (z == 0) fb.compare(x._2, y._2) else z
      }
    }

  implicit def catsDeferForOrder: Defer[Order] = OrderInstances.catsDeferForOrderCache
}
object OrderInstances {
  private val catsDeferForOrderCache: Defer[Order] =
    new Defer[Order] {
      case class Deferred[A](fa: () => Order[A]) extends Order[A] {
        private lazy val resolved: Order[A] = {
          @tailrec
          def loop(f: () => Order[A]): Order[A] =
            f() match {
              case Deferred(f) => loop(f)
              case next        => next
            }

          loop(fa)
        }
        override def compare(x: A, y: A): Int = resolved.compare(x, y)
      }

      override def defer[A](fa: => Order[A]): Order[A] = {
        lazy val cachedFa = fa
        Deferred(() => cachedFa)
      }
    }
}
