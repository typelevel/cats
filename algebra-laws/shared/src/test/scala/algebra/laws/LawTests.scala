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

import algebra.lattice._
import algebra.ring._
import algebra.instances.all._
import algebra.instances.BigDecimalAlgebra

import algebra.laws.platform.Platform

import org.scalacheck.{Arbitrary, Cogen}
import Arbitrary.arbitrary
import scala.collection.immutable.BitSet
import scala.util.Random

class LawTests extends munit.DisciplineSuite {

  implicit val byteLattice: Lattice[Byte] = ByteMinMaxLattice
  implicit val shortLattice: Lattice[Short] = ShortMinMaxLattice
  implicit val intLattice: BoundedDistributiveLattice[Int] = IntMinMaxLattice
  implicit val longLattice: BoundedDistributiveLattice[Long] = LongMinMaxLattice

  implicit def logicLaws[A: Eq: Arbitrary]: LogicLaws[A] = LogicLaws[A]

  implicit def latticeLaws[A: Eq: Arbitrary]: LatticeLaws[A] = LatticeLaws[A]
  implicit def ringLaws[A: Eq: Arbitrary: AdditiveMonoid]: RingLaws[A] = RingLaws[A]
  implicit def latticePartialOrderLaws[A: Eq: Arbitrary]: LatticePartialOrderLaws[A] = LatticePartialOrderLaws[A]

  case class HasEq[A](a: A)

  object HasEq {
    implicit def hasEq[A: Eq]: Eq[HasEq[A]] = Eq.by(_.a)
    implicit def hasEqArbitrary[A: Arbitrary]: Arbitrary[HasEq[A]] =
      Arbitrary(arbitrary[A].map(HasEq(_)))
    implicit def hasEqCogen[A: Cogen]: Cogen[HasEq[A]] =
      Cogen[A].contramap[HasEq[A]](_.a)
  }

  case class HasPartialOrder[A](a: A)

  object HasPartialOrder {
    implicit def hasPartialOrder[A: PartialOrder]: PartialOrder[HasPartialOrder[A]] = PartialOrder.by(_.a)
    implicit def hasPartialOrderArbitrary[A: Arbitrary]: Arbitrary[HasPartialOrder[A]] =
      Arbitrary(arbitrary[A].map(HasPartialOrder(_)))
    implicit def hasPartialOrderCogen[A: Cogen]: Cogen[HasPartialOrder[A]] =
      Cogen[A].contramap[HasPartialOrder[A]](_.a)
  }

  checkAll("Boolean", LogicLaws[Boolean].bool)
  checkAll("SimpleHeyting", DeMorganLaws[SimpleHeyting].logic(Logic.fromHeyting(Heyting[SimpleHeyting])))
  checkAll("SimpleHeyting", LogicLaws[SimpleHeyting].heyting)
  checkAll("SimpleDeMorgan", DeMorganLaws[SimpleDeMorgan].deMorgan)
  checkAll("Boolean", DeMorganLaws[Boolean].deMorgan(DeMorgan.fromBool(Bool[Boolean])))
  checkAll("Boolean", LatticePartialOrderLaws[Boolean].boundedLatticePartialOrder)
  checkAll("Boolean", RingLaws[Boolean].boolRing(booleanRing))

  // ensure that Bool[A].asBoolRing is a valid BoolRing
  checkAll("Boolean-ring-from-bool", RingLaws[Boolean].boolRing(new BoolRingFromBool[Boolean](Bool[Boolean])))

  // ensure that BoolRing[A].asBool is a valid Bool
  checkAll("Boolean- bool-from-ring", LogicLaws[Boolean].bool(new BoolFromBoolRing(booleanRing)))

  checkAll("Set[Byte]", LogicLaws[Set[Byte]].generalizedBool)
  checkAll("Set[Byte]", RingLaws[Set[Byte]].boolRng(setBoolRng[Byte]))
  checkAll("Set[Byte]-bool-from-rng", LogicLaws[Set[Byte]].generalizedBool(new GenBoolFromBoolRng(setBoolRng)))
  checkAll("Set[Byte]-rng-from-bool", RingLaws[Set[Byte]].boolRng(new BoolRngFromGenBool(GenBool[Set[Byte]])))
  checkAll("Set[Int]", RingLaws[Set[Int]].semiring)
  checkAll("Set[String]", RingLaws[Set[String]].semiring)

  checkAll("Map[Char, Int]", RingLaws[Map[Char, Int]].semiring)
  checkAll("Map[Int, BigInt]", RingLaws[Map[Int, BigInt]].semiring)

  checkAll("Byte", RingLaws[Byte].commutativeRing)
  checkAll("Byte", LatticeLaws[Byte].lattice)

  checkAll("Short", RingLaws[Short].commutativeRing)
  checkAll("Short", LatticeLaws[Short].lattice)

  checkAll("Int", RingLaws[Int].commutativeRing)
  checkAll("Int", LatticeLaws[Int].boundedDistributiveLattice)

  {
    checkAll("Int", RingLaws[Int].commutativeRig)
  }

  checkAll("Long", RingLaws[Long].commutativeRing)
  checkAll("Long", LatticeLaws[Long].boundedDistributiveLattice)

  checkAll("BigInt", RingLaws[BigInt].euclideanRing)

  checkAll("FPApprox[Float]", RingLaws[FPApprox[Float]].approxField)
  checkAll("FPApprox[Double]", RingLaws[FPApprox[Double]].approxField)

  // let's limit our BigDecimal-related tests to the JVM for now.
  if (Platform.isJvm) {

    {
      // we need a less intense arbitrary big decimal implementation.
      // this keeps the values relatively small/simple and avoids some
      // of the numerical errors we might hit.
      implicit val arbBigDecimal: Arbitrary[BigDecimal] =
        Arbitrary(arbitrary[Int].map(x => BigDecimal(x, java.math.MathContext.UNLIMITED)))

      // BigDecimal does have numerical errors, so we can't pass all of
      // the field laws.
      checkAll("BigDecimal", RingLaws[BigDecimal].ring)
    }

    {
      // We check the full field laws using a FPApprox.
      val mc = java.math.MathContext.DECIMAL32
      implicit val arbBigDecimal: Arbitrary[BigDecimal] =
        Arbitrary(arbitrary[Double].map(x => BigDecimal(x, mc)))
      implicit val epsBigDecimal = FPApprox.Epsilon.bigDecimalEpsilon(mc)
      implicit val algebra: FPApproxAlgebra[BigDecimal] =
        FPApprox.fpApproxAlgebra(new BigDecimalAlgebra(mc), Order[BigDecimal], epsBigDecimal)
      checkAll("FPApprox[BigDecimal]", RingLaws[FPApprox[BigDecimal]].field(algebra))
    }
  } else ()

  {
    implicit val arbBitSet: Arbitrary[BitSet] =
      Arbitrary(arbitrary[List[Byte]].map(s => BitSet(s.map(_ & 0xff): _*)))
    checkAll("BitSet", LogicLaws[BitSet].generalizedBool)
  }

  checkAll("(Int, Int)", RingLaws[(Int, Int)].ring)

  checkAll("Unit", RingLaws[Unit].commutativeRing)
  checkAll("Unit", RingLaws[Unit].multiplicativeMonoid)
  checkAll("Unit", LatticeLaws[Unit].boundedSemilattice)

  {
    // In order to check the monoid laws for `Order[N]`, we need
    // `Arbitrary[Order[N]]` and `Eq[Order[N]]` instances.
    // Here we have a bit of a hack to create these instances.
    val nMax: Int = 13
    final case class N(n: Int) { require(n >= 0 && n < nMax) }
    // The arbitrary `Order[N]` values are created by mapping N values to random
    // integers.
    implicit val arbNOrder: Arbitrary[Order[N]] = Arbitrary(arbitrary[Int].map { seed =>
      val order = new Random(seed).shuffle(Vector.range(0, nMax))
      Order.by { (n: N) => order(n.n) }
    })
    // The arbitrary `Eq[N]` values are created by mapping N values to random
    // integers.
    implicit val arbNEq: Arbitrary[Eq[N]] = Arbitrary(arbitrary[Int].map { seed =>
      val mapping = new Random(seed).shuffle(Vector.range(0, nMax))
      Eq.by { (n: N) => mapping(n.n) }
    })
    // needed because currently we don't have Vector instances
    implicit val vectorNEq: Eq[Vector[N]] = Eq.fromUniversalEquals
    // The `Eq[Order[N]]` instance enumerates all possible `N` values in a
    // `Vector` and considers two `Order[N]` instances to be equal if they
    // result in the same sorting of that vector.
    implicit val NOrderEq: Eq[Order[N]] = Eq.by { (order: Order[N]) =>
      Vector.tabulate(nMax)(N).sorted(order.toOrdering)
    }
    implicit val NEqEq: Eq[Eq[N]] = (a, b) =>
      Iterator
        .tabulate(nMax)(N)
        .flatMap { x => Iterator.tabulate(nMax)(N).map((x, _)) }
        .forall { case (x, y) => a.eqv(x, y) == b.eqv(x, y) }

    implicit val monoidOrderN: Monoid[Order[N]] = Order.whenEqualMonoid[N]
    checkAll("Order[N]", GroupLaws[Order[N]].monoid)

    {
      implicit val bsEqN: BoundedSemilattice[Eq[N]] = Eq.allEqualBoundedSemilattice[N]
      checkAll("Eq[N]", GroupLaws[Eq[N]].boundedSemilattice)
    }
    {
      implicit val sEqN: Semilattice[Eq[N]] = Eq.anyEqualSemilattice[N]
      checkAll("Eq[N]", GroupLaws[Eq[N]].semilattice)
    }
  }

  // checkAll("Int", "fromOrdering", OrderLaws[Int].order(Order.fromOrdering[Int]))
  checkAll("Array[Int]", OrderLaws[Array[Int]].order)
  checkAll("Array[Int]", OrderLaws[Array[Int]].partialOrder)

  // Rational tests do not return on Scala-js, so we make them JVM only.
  if (Platform.isJvm) checkAll("Rat", RingLaws[Rat].field)
  else ()

  test("Field.fromDouble with subnormal") {
    val n = 1.9726888167225064e-308
    val bd = new java.math.BigDecimal(n)
    val unscaledValue = new BigInt(bd.unscaledValue)
    val expected =
      if (bd.scale > 0) {
        Ring[Rat].fromBigInt(unscaledValue) / Ring[Rat].fromBigInt(BigInt(10).pow(bd.scale))
      } else {
        Ring[Rat].fromBigInt(unscaledValue * BigInt(10).pow(-bd.scale))
      }
    assert(Field.fromDouble[Rat](n) == expected)
  }
}
