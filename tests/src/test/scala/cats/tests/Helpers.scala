package cats
package tests

import org.scalacheck.Arbitrary
import Arbitrary.arbitrary

import cats.kernel.{ CommutativeSemigroup, CommutativeMonoid, CommutativeGroup }
import cats.kernel.{ Band, Semilattice, BoundedSemilattice }

/**
 * Helpers provides new concrete types where we control exactly which
 * type class instances are available. For example, the SL type has:
 *
 *  - Semilattice[SL]
 *  - Arbitrary[SL]
 *  - Eq[SL]
 *
 * (All types in Helpers have Arbitrary and Eq instances.)
 *
 * These are useful when a type constructor (e.g. Function0) can
 * produce many different instances depending on which instances are
 * available for its type parameter.
 */
object Helpers {

  abstract class Arb[E](f: Int => E) {
    implicit val earb: Arbitrary[E] = Arbitrary(arbitrary[Int].map(f))
  }

  trait Q[E] {
    implicit val eeq: Eq[E] = Eq.fromUniversalEquals
  }

  abstract class Companion[E](f: Int => E) extends Arb[E](f) with Q[E]

  // Eq
  case class Eqed(n: Int)
  object Eqed extends Companion(new Eqed(_))

  // PartialOrder
  case class POrd(n: Int)
  object POrd extends Arb(new POrd(_)) {
    implicit object O extends PartialOrder[POrd] {
      def partialCompare(x: POrd, y: POrd): Double =
        if (x.n >= 0 && y.n >= 0) (x.n compare y.n).toDouble
        else if (x.n <= 0 && y.n <= 0) (y.n compare x.n).toDouble
        else Double.NaN
    }
  }

  // Order
  case class Ord(n: Int)
  object Ord extends Arb(new Ord(_)) {
    implicit object O extends Order[Ord] {
      def compare(x: Ord, y: Ord): Int = x.n compare y.n
    }
  }

  // Band
  case class Bnd(n: Int)
  object Bnd extends Companion(new Bnd(_)) {
    implicit object Alg extends Band[Bnd] {
      def combine(x: Bnd, y: Bnd): Bnd = Bnd(x.n & y.n)
    }
  }

  // Semilattice
  case class SL(n: Int)
  object SL extends Companion(new SL(_)) {
    implicit object Alg extends Semilattice[SL] {
      def combine(x: SL, y: SL): SL = SL(x.n & y.n)
    }
  }

  // BoundedSemilattice
  case class BSL(n: Int)
  object BSL extends Companion(new BSL(_)) {
    implicit object Alg extends BoundedSemilattice[BSL] {
      def empty: BSL = BSL(0)
      def combine(x: BSL, y: BSL): BSL = BSL(x.n | y.n)
    }
  }

  // Semigroup
  case class Semi(n: Int)
  object Semi extends Companion(new Semi(_)) {
    implicit object Alg extends Semigroup[Semi] {
      def combine(x: Semi, y: Semi): Semi = Semi(x.n ^ y.n)
    }
  }

  // CommutativeSemigroup
  case class CSemi(n: Int)
  object CSemi extends Companion(new CSemi(_)) {
    implicit object Alg extends CommutativeSemigroup[CSemi] {
      def combine(x: CSemi, y: CSemi): CSemi = CSemi(x.n ^ y.n)
    }
  }

  // Monoid
  case class Mono(n: Int)
  object Mono extends Companion(new Mono(_)) {
    implicit object Alg extends Monoid[Mono] {
      def empty: Mono = Mono(Int.MaxValue)
      def combine(x: Mono, y: Mono): Mono = Mono(x.n min y.n)
    }
  }

  // CommutativeMonoid
  case class CMono(n: Int)
  object CMono extends Companion(new CMono(_)) {
    implicit object Alg extends CommutativeMonoid[CMono] {
      def empty: CMono = CMono(Int.MaxValue)
      def combine(x: CMono, y: CMono): CMono = CMono(x.n min y.n)
    }
  }

  // Group
  case class Grp(n: Int)
  object Grp extends Companion(new Grp(_)) {
    implicit object Alg extends Group[Grp] {
      def empty: Grp = Grp(0)
      def combine(x: Grp, y: Grp): Grp = Grp(x.n + y.n)
      def inverse(x: Grp): Grp = Grp(-x.n)
    }
  }

  // CommutativeGroup
  case class CGrp(n: Int)
  object CGrp extends Companion(new CGrp(_)) {
    implicit object Alg extends CommutativeGroup[CGrp] {
      def empty: CGrp = CGrp(0)
      def combine(x: CGrp, y: CGrp): CGrp = CGrp(x.n + y.n)
      def inverse(x: CGrp): CGrp = CGrp(-x.n)
    }
  }
}
