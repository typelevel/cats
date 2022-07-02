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

trait EquivInstances extends EquivInstances0 {
  implicit def catsDecidableForEquiv: Decidable[Equiv] =
    new Decidable[Equiv] {

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

      def sum[A, B](fa: Equiv[A], fb: Equiv[B]): Equiv[Either[A, B]] =
        new Equiv[Either[A, B]] {
          def equiv(x: Either[A, B], y: Either[A, B]): Boolean = (x, y) match {
            case (Left(a1), Left(a2))   => fa.equiv(a1, a2)
            case (Right(b1), Right(b2)) => fb.equiv(b1, b2)
            case _                      => false
          }
        }

      override val zero: Equiv[Nothing] = Equiv.by[Nothing, Unit](_ => ())
    }

    val catsContravariantMonoidalForEquiv: ContravariantMonoidal[Equiv] = 
      catsContravariantMonoidalForEquivForBinCompat
}

private[instances] trait EquivInstances0 {
  implicit def catsContravariantMonoidalForEquivForBinCompat: ContravariantMonoidal[Equiv] =
    new ContravariantMonoidal[Equiv] {

      /**
       * Defaults to trivially contracting the type
       * to a point
       */
      val unit: Equiv[Unit] =
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
}
