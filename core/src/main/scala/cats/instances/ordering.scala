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

trait OrderingInstances {
  implicit val catsContravariantMonoidalForOrdering: ContravariantMonoidal[Ordering] =
    new ContravariantMonoidal[Ordering] {

      /**
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def unit: Ordering[Unit] = Order[Unit].toOrdering

      def contramap[A, B](fa: Ordering[A])(f: B => A): Ordering[B] = fa.on(f)

      def product[A, B](fa: Ordering[A], fb: Ordering[B]): Ordering[(A, B)] =
        new Ordering[(A, B)] {
          def compare(x: (A, B), y: (A, B)): Int = {
            val z = fa.compare(x._1, y._1)
            if (z == 0) fb.compare(x._2, y._2) else z
          }
        }
    }

  implicit def catsStdDeferForOrdering: Defer[Ordering] = OrderingInstances.catsStdDeferForOrderingCache
}
object OrderingInstances {
  private val catsStdDeferForOrderingCache: Defer[Ordering] =
    new Defer[Ordering] {
      case class Deferred[A](fa: () => Ordering[A]) extends Ordering[A] {
        private lazy val resolved: Ordering[A] = {
          @tailrec
          def loop(f: () => Ordering[A]): Ordering[A] =
            f() match {
              case Deferred(f) => loop(f)
              case next        => next
            }

          loop(fa)
        }
        override def compare(x: A, y: A): Int = resolved.compare(x, y)
      }

      override def defer[A](fa: => Ordering[A]): Ordering[A] = {
        lazy val cachedFa = fa
        Deferred(() => cachedFa)
      }
    }
}
