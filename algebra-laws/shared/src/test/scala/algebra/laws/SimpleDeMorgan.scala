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

package algebra
package laws

import algebra.lattice.DeMorgan

import org.scalacheck.Arbitrary
import org.scalacheck.Gen.oneOf

/**
 * The simplest De Morgan algebra that is not already a Boolean algebra.
 * It is the standard three valued logic.
 * Taken from https://en.wikipedia.org/wiki/De_Morgan_algebra#Kleene_algebra
 */
sealed trait SimpleDeMorgan

object SimpleDeMorgan {
  private case object False extends SimpleDeMorgan
  private case object Unknown extends SimpleDeMorgan
  private case object True extends SimpleDeMorgan

  implicit val deMorgan: DeMorgan[SimpleDeMorgan] = new DeMorgan[SimpleDeMorgan] {
    def zero: SimpleDeMorgan = False
    def one: SimpleDeMorgan = True

    def and(a: SimpleDeMorgan, b: SimpleDeMorgan): SimpleDeMorgan = (a, b) match {
      case (False, _)   => False
      case (_, False)   => False
      case (Unknown, _) => Unknown
      case (_, Unknown) => Unknown
      case _            => True
    }

    def or(a: SimpleDeMorgan, b: SimpleDeMorgan): SimpleDeMorgan = (a, b) match {
      case (False, x)   => x
      case (x, False)   => x
      case (Unknown, x) => x
      case (x, Unknown) => x
      case _            => True
    }

    def not(a: SimpleDeMorgan): SimpleDeMorgan = a match {
      case False   => True
      case Unknown => Unknown
      case True    => False
    }
  }

  implicit val arbitrary: Arbitrary[SimpleDeMorgan] = Arbitrary(oneOf(False, Unknown, True))

  implicit val eq: Eq[SimpleDeMorgan] =
    Eq.fromUniversalEquals
}
