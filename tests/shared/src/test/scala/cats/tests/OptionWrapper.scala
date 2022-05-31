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

import cats.Functor
import cats.laws.discipline.ExhaustiveCheck
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Arbitrary.arbitrary

/**
 * Similar to [[ListWrapper]], but using `Option` instead of `List` limits the size of the structure, which can be
 * useful for limiting the space of test values to generate.
 */
final case class OptionWrapper[A](option: Option[A]) extends AnyVal

object OptionWrapper {
  val functor: Functor[OptionWrapper] = new Functor[OptionWrapper] {
    def map[A, B](fa: OptionWrapper[A])(f: A => B) = OptionWrapper(fa.option.map(f))
  }

  implicit def optionWrapperArbitrary[A: Arbitrary]: Arbitrary[OptionWrapper[A]] =
    Arbitrary(arbitrary[Option[A]].map(OptionWrapper.apply))

  implicit def optionWrapperCogen[A: Cogen]: Cogen[OptionWrapper[A]] =
    Cogen[Option[A]].contramap(_.option)

  implicit def catsLawsExhaustiveCheckForOptionWrapper[A](implicit
    A: ExhaustiveCheck[A]
  ): ExhaustiveCheck[OptionWrapper[A]] =
    ExhaustiveCheck.instance(ExhaustiveCheck[Option[A]].allValues.map(OptionWrapper(_)))
}
