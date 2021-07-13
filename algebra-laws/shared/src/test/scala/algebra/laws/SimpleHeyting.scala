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

  implicit val eq: Eq[SimpleHeyting] = new Eq[SimpleHeyting] {
    def eqv(x: SimpleHeyting, y: SimpleHeyting): Boolean = x == y
  }
}
