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

import algebra.lattice.Heyting

import org.scalacheck.Arbitrary
import org.scalacheck.Gen.oneOf

/**
 * The simplest Heyting algebra that is not already a Boolean algebra.
 * Taken from https://en.wikipedia.org/wiki/Heyting_algebra#Examples
 */
sealed trait SimpleHeyting

object SimpleHeyting {
  private case object Zero extends SimpleHeyting
  private case object Half extends SimpleHeyting
  private case object One extends SimpleHeyting

  implicit val heyting: Heyting[SimpleHeyting] = new Heyting[SimpleHeyting] {
    def zero: SimpleHeyting = Zero
    def one: SimpleHeyting = One

    def and(a: SimpleHeyting, b: SimpleHeyting): SimpleHeyting = (a, b) match {
      case (Zero, _) => Zero
      case (_, Zero) => Zero
      case (Half, _) => Half
      case (_, Half) => Half
      case _         => One
    }

    def or(a: SimpleHeyting, b: SimpleHeyting): SimpleHeyting = (a, b) match {
      case (Zero, x) => x
      case (x, Zero) => x
      case (Half, x) => x
      case (x, Half) => x
      case _         => One
    }

    def complement(a: SimpleHeyting): SimpleHeyting = a match {
      case Zero => One
      case Half => Zero
      case One  => Zero
    }

    def imp(a: SimpleHeyting, b: SimpleHeyting): SimpleHeyting = (a, b) match {
      case (Zero, _) => One
      case (_, Zero) => Zero
      case (Half, _) => One
      case (One, x)  => x
    }
  }

  implicit val arbitrary: Arbitrary[SimpleHeyting] = Arbitrary(oneOf(Zero, Half, One))

  implicit val eq: Eq[SimpleHeyting] =
    Eq.fromUniversalEquals
}
