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

import cats.kernel.*
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Arbitrary.arbitrary

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

  abstract class N { def n: Int }

  abstract class Arb[E <: N](f: Int => E) {
    implicit val earb: Arbitrary[E] = Arbitrary(arbitrary[Int].map(f))
    implicit val ccog: Cogen[E] = Cogen[Int].contramap(_.n)
  }

  trait Q[E] {
    implicit val eeq: Eq[E] = Eq.fromUniversalEquals
  }

  abstract class Companion[E <: N](f: Int => E) extends Arb[E](f) with Q[E]

  // Eq
  case class Eqed(n: Int) extends N
  object Eqed extends Companion(new Eqed(_))

  // PartialOrder
  case class POrd(n: Int) extends N
  object POrd extends Arb(new POrd(_)) {
    implicit object O extends PartialOrder[POrd] {
      def partialCompare(x: POrd, y: POrd): Double =
        if (x.n >= 0 && y.n >= 0) x.n.compare(y.n).toDouble
        else if (x.n <= 0 && y.n <= 0) y.n.compare(x.n).toDouble
        else Double.NaN
    }
  }

  // Order
  case class Ord(n: Int) extends N
  object Ord extends Arb(new Ord(_)) {
    implicit object O extends Order[Ord] {
      def compare(x: Ord, y: Ord): Int = x.n.compare(y.n)
    }
  }

  case class Hsh(n: Int) extends N
  object Hsh extends Arb(new Hsh(_)) {
    implicit object O extends Hash[Hsh] {
      def hash(x: Hsh): Int = x.hashCode()
      def eqv(x: Hsh, y: Hsh): Boolean = x.n == y.n
    }
  }

  // Band
  case class Bnd(n: Int) extends N
  object Bnd extends Companion(new Bnd(_)) {
    implicit object Alg extends Band[Bnd] {
      def combine(x: Bnd, y: Bnd): Bnd = Bnd(x.n & y.n)
    }
  }

  // Semilattice
  case class SL(n: Int) extends N
  object SL extends Companion(new SL(_)) {
    implicit object Alg extends Semilattice[SL] {
      def combine(x: SL, y: SL): SL = SL(x.n & y.n)
    }
  }

  // BoundedSemilattice
  case class BSL(n: Int) extends N
  object BSL extends Companion(new BSL(_)) {
    implicit object Alg extends BoundedSemilattice[BSL] {
      def empty: BSL = BSL(0)
      def combine(x: BSL, y: BSL): BSL = BSL(x.n | y.n)
    }
  }

  // Semigroup
  case class Semi(n: Int) extends N
  object Semi extends Companion(new Semi(_)) {
    implicit object Alg extends Semigroup[Semi] {
      def combine(x: Semi, y: Semi): Semi = Semi(x.n ^ y.n)
    }
  }

  // CommutativeSemigroup
  case class CSemi(n: Int) extends N
  object CSemi extends Companion(new CSemi(_)) {
    implicit object Alg extends CommutativeSemigroup[CSemi] {
      def combine(x: CSemi, y: CSemi): CSemi = CSemi(x.n ^ y.n)
    }
  }

  // Monoid
  case class Mono(n: Int) extends N
  object Mono extends Companion(new Mono(_)) {
    implicit object Alg extends Monoid[Mono] {
      def empty: Mono = Mono(Int.MaxValue)
      def combine(x: Mono, y: Mono): Mono = Mono(x.n.min(y.n))
    }
  }

  // CommutativeMonoid
  case class CMono(n: Int) extends N
  object CMono extends Companion(new CMono(_)) {
    implicit object Alg extends CommutativeMonoid[CMono] {
      def empty: CMono = CMono(Int.MaxValue)
      def combine(x: CMono, y: CMono): CMono = CMono(x.n.min(y.n))
    }
  }

  // Group
  case class Grp(n: Int) extends N
  object Grp extends Companion(new Grp(_)) {
    implicit object Alg extends Group[Grp] {
      def empty: Grp = Grp(0)
      def combine(x: Grp, y: Grp): Grp = Grp(x.n + y.n)
      def inverse(x: Grp): Grp = Grp(-x.n)
    }
  }

  // CommutativeGroup
  case class CGrp(n: Int) extends N
  object CGrp extends Companion(new CGrp(_)) {
    implicit object Alg extends CommutativeGroup[CGrp] {
      def empty: CGrp = CGrp(0)
      def combine(x: CGrp, y: CGrp): CGrp = CGrp(x.n + y.n)
      def inverse(x: CGrp): CGrp = CGrp(-x.n)
    }
  }
}
