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

package cats.instances

import cats.kernel._
import cats.kernel.instances.unit._
import cats.{Invariant, InvariantMonoidal, InvariantSemigroupal, Monoid}

trait InvariantMonoidalInstances {

  implicit def catsSemigroupalForMonoid: InvariantSemigroupal[Monoid] =
    new InvariantSemigroupal[Monoid] {
      def product[A, B](fa: Monoid[A], fb: Monoid[B]): Monoid[(A, B)] =
        new Monoid[(A, B)] {
          val empty = fa.empty -> fb.empty
          def combine(x: (A, B), y: (A, B)): (A, B) = fa.combine(x._1, y._1) -> fb.combine(x._2, y._2)
        }

      def imap[A, B](fa: Monoid[A])(f: A => B)(g: B => A): Monoid[B] =
        new Monoid[B] {
          def empty: B = f(fa.empty)

          def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
        }
    }

  implicit val catsInvariantMonoidalSemigroup: InvariantMonoidal[Semigroup] = new InvariantMonoidal[Semigroup] {
    def product[A, B](fa: Semigroup[A], fb: Semigroup[B]): Semigroup[(A, B)] =
      (x, y) => fa.combine(x._1, y._1) -> fb.combine(x._2, y._2)

    def imap[A, B](fa: Semigroup[A])(f: A => B)(g: B => A): Semigroup[B] =
      (x, y) => f(fa.combine(g(x), g(y)))

    def unit: Semigroup[Unit] = implicitly
  }

  implicit val catsInvariantMonoidalCommutativeSemigroup: InvariantMonoidal[CommutativeSemigroup] =
    new InvariantMonoidal[CommutativeSemigroup] {
      def product[A, B](fa: CommutativeSemigroup[A], fb: CommutativeSemigroup[B]): CommutativeSemigroup[(A, B)] =
        (x, y) => fa.combine(x._1, y._1) -> fb.combine(x._2, y._2)

      def imap[A, B](fa: CommutativeSemigroup[A])(f: A => B)(g: B => A): CommutativeSemigroup[B] =
        (x, y) => f(fa.combine(g(x), g(y)))

      def unit: CommutativeSemigroup[Unit] = implicitly
    }
}

trait InvariantInstances {
  implicit val catsInvariantForNumeric: Invariant[Numeric] = new Invariant[Numeric] {
    def imap[A, B](fa: Numeric[A])(f: A => B)(g: B => A): Numeric[B] =
      new ScalaVersionSpecificNumeric[A, B](fa)(f)(g) {}
  }

  implicit val catsInvariantForIntegral: Invariant[Integral] = new Invariant[Integral] {
    def imap[A, B](fa: Integral[A])(f: A => B)(g: B => A): Integral[B] =
      new ScalaVersionSpecificNumeric[A, B](fa)(f)(g) with Integral[B] {
        override def quot(x: B, y: B): B = f(fa.quot(g(x), g(y)))
        override def rem(x: B, y: B): B = f(fa.rem(g(x), g(y)))
      }
  }
}

trait InvariantInstancesBinCompat0 {
  implicit val catsInvariantForFractional: Invariant[Fractional] = new Invariant[Fractional] {
    def imap[A, B](fa: Fractional[A])(f: A => B)(g: B => A): Fractional[B] =
      new ScalaVersionSpecificNumeric[A, B](fa)(f)(g) with Fractional[B] {
        override def div(x: B, y: B): B = f(fa.div(g(x), g(y)))
      }
  }
}
