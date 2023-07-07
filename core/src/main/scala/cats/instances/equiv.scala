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

import scala.annotation.tailrec

trait EquivInstances {
  implicit val catsContravariantMonoidalForEquiv: ContravariantMonoidal[Equiv] =
    new ContravariantMonoidal[Equiv] {

      /**
       * Defaults to trivially contracting the type
       * to a point
       */
      def unit: Equiv[Unit] =
        new Equiv[Unit] {
          def equiv(x: Unit, y: Unit): Boolean = true
        }

      /**
       * Derive an `Equiv` for `B` given an `Equiv[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: Equiv[A])(f: B => A): Equiv[B] =
        new Equiv[B] {
          def equiv(l: B, r: B): Boolean =
            fa.equiv(f(l), f(r))
        }

      def product[A, B](fa: Equiv[A], fb: Equiv[B]): Equiv[(A, B)] =
        new Equiv[(A, B)] {
          def equiv(l: (A, B), r: (A, B)): Boolean =
            fa.equiv(l._1, r._1) && fb.equiv(l._2, r._2)
        }
    }

  implicit def catsDeferForEquiv: Defer[Equiv] = EquivInstances.catsDeferForEquivCache
}
object EquivInstances {
  private val catsDeferForEquivCache: Defer[Equiv] =
    new Defer[Equiv] {
      case class Deferred[A](fa: () => Equiv[A]) extends Equiv[A] {
        private lazy val resolved: Equiv[A] = {
          @tailrec
          def loop(f: () => Equiv[A]): Equiv[A] =
            f() match {
              case Deferred(f) => loop(f)
              case next        => next
            }

          loop(fa)
        }
        override def equiv(x: A, y: A): Boolean = resolved.equiv(x, y)
      }

      override def defer[A](fa: => Equiv[A]): Equiv[A] = {
        lazy val cachedFa = fa
        Deferred(() => cachedFa)
      }
    }
}
