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

trait PartialOrderingInstances {
  implicit val catsContravariantMonoidalForPartialOrdering: ContravariantMonoidal[PartialOrdering] =
    new ContravariantMonoidal[PartialOrdering] {

      /**
       * Derive a `PartialOrdering` for `B` given a `PartialOrdering[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: PartialOrdering[A])(f: B => A): PartialOrdering[B] =
        new PartialOrdering[B] {
          def lteq(x: B, y: B): Boolean = fa.lteq(f(x), f(y))
          def tryCompare(x: B, y: B): Option[Int] = fa.tryCompare(f(x), f(y))
        }

      def product[A, B](fa: PartialOrdering[A], fb: PartialOrdering[B]): PartialOrdering[(A, B)] =
        new PartialOrdering[(A, B)] {
          def lteq(x: (A, B), y: (A, B)): Boolean =
            tryCompare(x, y).exists(_ <= 0)
          def tryCompare(x: (A, B), y: (A, B)): Option[Int] =
            fa.tryCompare(x._1, y._1) match {
              case Some(0) => fb.tryCompare(x._2, y._2)
              case option  => option
            }
        }

      def unit: PartialOrdering[Unit] = cats.instances.unit.catsKernelStdOrderForUnit.toOrdering
    }

  implicit def catsStdDeferForPartialOrdering: Defer[PartialOrdering] =
    PartialOrderingInstances.catsStdDeferForPartialOrderingCache
}
object PartialOrderingInstances {
  private val catsStdDeferForPartialOrderingCache: Defer[PartialOrdering] =
    new Defer[PartialOrdering] {
      case class Deferred[A](fa: () => PartialOrdering[A]) extends PartialOrdering[A] {
        private lazy val resolve: PartialOrdering[A] = {
          @tailrec
          def loop(f: () => PartialOrdering[A]): PartialOrdering[A] =
            f() match {
              case Deferred(f) => loop(f)
              case next        => next
            }

          loop(fa)
        }

        override def tryCompare(x: A, y: A): Option[Int] = resolve.tryCompare(x, y)

        override def lteq(x: A, y: A): Boolean = resolve.lteq(x, y)
      }

      override def defer[A](fa: => PartialOrdering[A]): PartialOrdering[A] = {
        lazy val cachedFa = fa
        Deferred(() => cachedFa)
      }
    }
}
