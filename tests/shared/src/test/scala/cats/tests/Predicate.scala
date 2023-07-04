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

package cats.tests

import cats.Decidable
import cats.instances.function.catsStdDecidableForPredicate
import cats.kernel.Eq
import cats.laws.discipline.eq._
import cats.laws.discipline.ExhaustiveCheck
import org.scalacheck.{Arbitrary, Cogen}

case class Predicate[A](run: A => Boolean)
object Predicate {
  implicit val decidablePredicate: Decidable[Predicate] =
    new Decidable[Predicate] {
      def unit: Predicate[Unit] = Predicate[Unit](catsStdDecidableForPredicate.unit)
      def product[A, B](fa: Predicate[A], fb: Predicate[B]): Predicate[(A, B)] =
        Predicate(catsStdDecidableForPredicate.product(fa.run, fb.run))
      def contramap[A, B](fa: Predicate[A])(f: B => A): Predicate[B] =
        Predicate(catsStdDecidableForPredicate.contramap(fa.run)(f))
      def sum[A, B](fa: Predicate[A], fb: Predicate[B]): Predicate[Either[A, B]] =
        Predicate(catsStdDecidableForPredicate.sum(fa.run, fb.run))
      def zero: Predicate[Nothing] = Predicate[Nothing](catsStdDecidableForPredicate.zero)
    }

  implicit def eqPredicate[A: ExhaustiveCheck]: Eq[Predicate[A]] =
    Eq.by[Predicate[A], A => Boolean](_.run)

  implicit def arbPredicate[A: Cogen]: Arbitrary[Predicate[A]] =
    Arbitrary(implicitly[Arbitrary[A => Boolean]].arbitrary.map(f => Predicate(f)))
}
