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

  implicit val eq: Eq[SimpleDeMorgan] = new Eq[SimpleDeMorgan] {
    def eqv(x: SimpleDeMorgan, y: SimpleDeMorgan): Boolean = x == y
  }
}
