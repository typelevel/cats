package cats
package algebra
package laws

import cats.algebra.lattice._
import cats.algebra.ring._
import cats.algebra.instances.all._
import cats.platform.Platform
import org.scalacheck.{Arbitrary, Cogen}
import Arbitrary.arbitrary
import cats.algebra.instances.BigDecimalAlgebra

import scala.collection.immutable.BitSet

class LawTests extends munit.DisciplineSuite {

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

  checkAll("Boolean", LogicTests[Boolean].bool)
  checkAll("SimpleHeyting", LogicTests[SimpleHeyting].logic(Logic.fromHeyting(Heyting[SimpleHeyting])))
  checkAll("SimpleHeyting", LogicTests[SimpleHeyting].heyting)
  checkAll("SimpleDeMorgan", LogicTests[SimpleDeMorgan].deMorgan)
  checkAll("Boolean", LogicTests[Boolean].deMorgan(DeMorgan.fromBool(Bool[Boolean])))
  checkAll("Boolean", LatticePartialOrderTests[Boolean].boundedLatticePartialOrder)
  checkAll("Boolean", RingTests[Boolean].boolRing(catsAlgebraStdRingForBoolean))

  // ensure that Bool[A].asBoolRing is a valid BoolRing
  checkAll("Boolean-ring-from-bool", RingTests[Boolean].boolRing(new BoolRingFromBool[Boolean](Bool[Boolean])))

  // ensure that BoolRing[A].asBool is a valid Bool
  checkAll("Boolean- bool-from-ring", LogicTests[Boolean].bool(new BoolFromBoolRing(catsAlgebraStdRingForBoolean)))

  checkAll("Set[Byte]", LogicTests[Set[Byte]].generalizedBool)
  checkAll("Set[Byte]", RingTests[Set[Byte]].boolRng(catsAlgebraStdBoolRngForSet))
  checkAll("Set[Byte]-bool-from-rng",
           LogicTests[Set[Byte]].generalizedBool(new GenBoolFromBoolRng(catsAlgebraStdBoolRngForSet))
  )
  checkAll("Set[Byte]-rng-from-bool", RingTests[Set[Byte]].boolRng(new BoolRngFromGenBool(GenBool[Set[Byte]])))
  checkAll("Set[Int]", RingTests[Set[Int]].semiring)
  checkAll("Set[String]", RingTests[Set[String]].semiring)

  checkAll("Map[Char, Int]", RingTests[Map[Char, Int]].semiring)
  checkAll("Map[Int, BigInt]", RingTests[Map[Int, BigInt]].semiring)

  checkAll("Byte", RingTests[Byte].commutativeRing)
  checkAll("Byte", LatticeTests[Byte].lattice(catsAlgebraStdMinMaxLatticeForByte))

  checkAll("Short", RingTests[Short].commutativeRing)
  checkAll("Short", LatticeTests[Short].lattice(catsAlgebraStdMinMaxLatticeForShort))

  checkAll("Int", RingTests[Int].commutativeRing)
  checkAll("Int", LatticeTests[Int].boundedDistributiveLattice(catsAlgebraStdBoundedDistributiveLatticeForInt))

  {
    checkAll("Int", RingTests[Int].commutativeRig)
  }

  checkAll("Long", RingTests[Long].commutativeRing)
  checkAll("Long", LatticeTests[Long].boundedDistributiveLattice(catsAlgebraStdBoundedDistributiveLatticeForLong))

  checkAll("BigInt", RingTests[BigInt].euclideanRing)
  checkAll("BigInt", SignedTests[BigInt].truncatedDivision)
  checkAll("BigInt", SignedTests[BigInt].signedGCDRing)

  checkAll("FPApprox[Float]", RingTests[FPApprox[Float]].approxField)
  checkAll("FPApprox[Double]", RingTests[FPApprox[Double]].approxField)

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
      checkAll("BigDecimal", RingTests[BigDecimal].ring)
    }

    {
      // We check the full field laws using a FPApprox.
      val mc = java.math.MathContext.DECIMAL32
      implicit val arbBigDecimal: Arbitrary[BigDecimal] =
        Arbitrary(arbitrary[Double].map(x => BigDecimal(x, mc)))
      implicit val epsBigDecimal = FPApprox.Epsilon.bigDecimalEpsilon(mc)
      implicit val algebra: FPApproxAlgebra[BigDecimal] =
        FPApprox.fpApproxAlgebra(new BigDecimalAlgebra(mc), Order[BigDecimal], epsBigDecimal)
      checkAll("FPApprox[BigDecimal]", RingTests[FPApprox[BigDecimal]].field(algebra))
    }
  } else ()

  {
    implicit val arbBitSet: Arbitrary[BitSet] =
      Arbitrary(arbitrary[List[Byte]].map(s => BitSet(s.map(_ & 0xff): _*)))
    checkAll("BitSet", LogicTests[BitSet].generalizedBool)
  }

  checkAll("(Int, Int)", RingTests[(Int, Int)].ring)

  checkAll("Unit", RingTests[Unit].commutativeRing)
  checkAll("Unit", RingTests[Unit].multiplicativeMonoid)

  // Rational tests do not return on Scala-js, so we make them JVM only.
  if (Platform.isJvm) checkAll("Rat", RingTests[Rat].field)
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
