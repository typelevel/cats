package cats
package algebra
package laws

import cats.algebra.lattice._
import cats.algebra.ring._
import cats.algebra.instances.all._
import cats.algebra.instances.BigDecimalAlgebra

import cats.algebra.laws.platform.Platform

import org.scalacheck.{Arbitrary, Cogen}
import Arbitrary.arbitrary
import scala.collection.immutable.BitSet
import scala.util.Random

class LawTests extends munit.DisciplineSuite {

  implicit val byteLattice: Lattice[Byte] = ByteMinMaxLattice
  implicit val shortLattice: Lattice[Short] = ShortMinMaxLattice
  implicit val intLattice: BoundedDistributiveLattice[Int] = IntMinMaxLattice
  implicit val longLattice: BoundedDistributiveLattice[Long] = LongMinMaxLattice

  implicit def orderLaws[A: Cogen: Eq: Arbitrary]: OrderLaws[A] = OrderLaws[A]
  implicit def groupLaws[A: Eq: Arbitrary]: GroupLaws[A] = GroupLaws[A]
  implicit def logicLaws[A: Eq: Arbitrary]: LogicLaws[A] = LogicLaws[A]
  implicit def deMorganLaws[A: Eq: Arbitrary]: DeMorganLaws[A] = DeMorganLaws[A]

  implicit def latticeLaws[A: Eq: Arbitrary]: LatticeLaws[A] = LatticeLaws[A]
  implicit def ringLaws[A: Eq: Arbitrary: AdditiveMonoid]: RingLaws[A] = RingLaws[A]
  implicit def baseLaws[A: Eq: Arbitrary]: BaseLaws[A] = BaseLaws[A]
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

  checkAll("Boolean", OrderLaws[Boolean].order) //("Boolean").check(_.order)
  checkAll("Boolean", LogicLaws[Boolean].bool)
  checkAll("SimpleHeyting", DeMorganLaws[SimpleHeyting].logic(Logic.fromHeyting(Heyting[SimpleHeyting])))
  checkAll("SimpleHeyting", LogicLaws[SimpleHeyting].heyting)
  checkAll("SimpleDeMorgan", DeMorganLaws[SimpleDeMorgan].deMorgan)
  checkAll("Boolean", DeMorganLaws[Boolean].deMorgan(DeMorgan.fromBool(Bool[Boolean])))
  checkAll("Boolean", LatticePartialOrderLaws[Boolean].boundedLatticePartialOrder)
  checkAll("Boolean", RingLaws[Boolean].boolRing(booleanRing))

  // ensure that Bool[A].asBoolRing is a valid BoolRing
  checkAll("Boolean-ring-from-bool", RingLaws[Boolean].boolRing(Bool[Boolean].asBoolRing))

  // ensure that BoolRing[A].asBool is a valid Bool
  checkAll("Boolean- bool-from-ring", LogicLaws[Boolean].bool(new BoolFromBoolRing(booleanRing)))

  checkAll("String", OrderLaws[String].order)
  checkAll("String", GroupLaws[String].monoid)

  {
    checkAll("Option[HasEq[Int]]", OrderLaws[Option[HasEq[Int]]].eqv)
    checkAll("Option[HasPartialOrder[Int]]", OrderLaws[Option[HasPartialOrder[Int]]].partialOrder)
    checkAll("Option[Int]", OrderLaws[Option[Int]].order)
    checkAll("Option[Int]", GroupLaws[Option[Int]].monoid)
    checkAll("Option[HasEq[String]]", OrderLaws[Option[HasEq[String]]].eqv)
    checkAll("Option[HasPartialOrder[String]]", OrderLaws[Option[HasPartialOrder[String]]].partialOrder)
    checkAll("Option[String]", OrderLaws[Option[String]].order)
    checkAll("Option[String]", GroupLaws[Option[String]].monoid)
  }

  checkAll("List[HasEq[Int]]", OrderLaws[List[HasEq[Int]]].eqv)
  checkAll("List[HasPartialOrder[Int]]", OrderLaws[List[HasPartialOrder[Int]]].partialOrder)
  checkAll("List[Int]", OrderLaws[List[Int]].order)
  checkAll("List[Int]", GroupLaws[List[Int]].monoid)
  checkAll("List[HasEq[String]]", OrderLaws[List[HasEq[String]]].eqv)
  checkAll("List[HasPartialOrder[String]]", OrderLaws[List[HasPartialOrder[String]]].partialOrder)
  checkAll("List[String]", OrderLaws[List[String]].order)
  checkAll("List[String]", GroupLaws[List[String]].monoid)

  checkAll("Set[Byte]", LogicLaws[Set[Byte]].generalizedBool)
  checkAll("Set[Byte]", RingLaws[Set[Byte]].boolRng(setBoolRng[Byte]))
  checkAll("Set[Byte]-bool-from-rng", LogicLaws[Set[Byte]].generalizedBool(new GenBoolFromBoolRng(setBoolRng)))
  checkAll("Set[Byte]-rng-from-bool", RingLaws[Set[Byte]].boolRng(GenBool[Set[Byte]].asBoolRing))
  checkAll("Set[Int]", OrderLaws[Set[Int]].partialOrder)
  checkAll("Set[Int]", RingLaws[Set[Int]].semiring)
  checkAll("Set[String]", RingLaws[Set[String]].semiring)

  checkAll("Map[Char, Int]", OrderLaws[Map[Char, Int]].eqv)
  checkAll("Map[Char, Int]", RingLaws[Map[Char, Int]].semiring)
  checkAll("Map[Int, BigInt]", OrderLaws[Map[Int, BigInt]].eqv)
  checkAll("Map[Int, BigInt]", RingLaws[Map[Int, BigInt]].semiring)

  checkAll("Byte", OrderLaws[Byte].order)
  checkAll("Byte", RingLaws[Byte].commutativeRing)
  checkAll("Byte", LatticeLaws[Byte].lattice)

  checkAll("Short", OrderLaws[Short].order)
  checkAll("Short", RingLaws[Short].commutativeRing)
  checkAll("Short", LatticeLaws[Short].lattice)

  checkAll("Char", OrderLaws[Char].order)

  checkAll("Int", OrderLaws[Int].order)
  checkAll("Int", RingLaws[Int].commutativeRing)
  checkAll("Int", LatticeLaws[Int].boundedDistributiveLattice)

  {
    checkAll("Int", RingLaws[Int].commutativeRig)
  }

  checkAll("Long", OrderLaws[Long].order)
  checkAll("Long", RingLaws[Long].commutativeRing)
  checkAll("Long", LatticeLaws[Long].boundedDistributiveLattice)

  checkAll("BigInt", RingLaws[BigInt].commutativeRing)

  checkAll("FPApprox[Float]", RingLaws[FPApprox[Float]].field)
  checkAll("FPApprox[Double]", RingLaws[FPApprox[Double]].field)

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

  {
    implicit val band = new Band[(Int, Int)] {
      def combine(a: (Int, Int), b: (Int, Int)) = (a._1, b._2)
    }
    checkAll("(Int, Int) Band", GroupLaws[(Int, Int)].band)
  }

  checkAll("Unit", OrderLaws[Unit].order)
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
    implicit val NEqEq: Eq[Eq[N]] = new Eq[Eq[N]] {
      def eqv(a: Eq[N], b: Eq[N]) =
        Iterator
          .tabulate(nMax)(N)
          .flatMap { x => Iterator.tabulate(nMax)(N).map((x, _)) }
          .forall { case (x, y) => a.eqv(x, y) == b.eqv(x, y) }
    }

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
