package cats.kernel
package laws

import catalysts.Platform
import catalysts.macros.TypeTagM
import cats.kernel.instances.all._
import cats.kernel.laws.discipline._

// these aren't included in all due to bincompat
import cats.kernel.instances.duration._
import cats.kernel.instances.queue._

import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.Discipline
import org.scalacheck.{ Arbitrary, Cogen, Gen }
import Arbitrary.arbitrary
import org.scalactic.anyvals.{ PosInt, PosZInt }
import org.scalatest.FunSuite

import scala.concurrent.duration.Duration
import scala.collection.immutable.{BitSet, Queue}
import scala.util.Random

import java.util.UUID
import java.util.concurrent.TimeUnit.{DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS, MICROSECONDS, NANOSECONDS}

object KernelCheck {

  implicit val arbitraryBitSet: Arbitrary[BitSet] =
    Arbitrary(arbitrary[List[Short]].map(ns => BitSet(ns.map(_ & 0xffff): _*)))

  implicit val arbitrarySymbol: Arbitrary[Symbol] =
    Arbitrary(arbitrary[String].map(s => Symbol(s)))

  implicit val arbitraryUUID: Arbitrary[UUID] =
    Arbitrary(Gen.uuid)

  implicit val arbitraryDuration: Arbitrary[Duration] = {
    // max range is +/- 292 years, but we give ourselves some extra headroom
    // to ensure that we can add these things up. they crash on overflow.
    val n = (292L * 365) / 50
    Arbitrary(Gen.oneOf(
      Gen.choose(-n, n).map(Duration(_, DAYS)),
      Gen.choose(-n * 24L, n * 24L).map(Duration(_, HOURS)),
      Gen.choose(-n * 1440L, n * 1440L).map(Duration(_, MINUTES)),
      Gen.choose(-n * 86400L, n * 86400L).map(Duration(_, SECONDS)),
      Gen.choose(-n * 86400000L, n * 86400000L).map(Duration(_, MILLISECONDS)),
      Gen.choose(-n * 86400000000L, n * 86400000000L).map(Duration(_, MICROSECONDS)),
      Gen.choose(-n * 86400000000000L, n * 86400000000000L).map(Duration(_, NANOSECONDS))))
  }

  // this instance is not available in scalacheck 1.13.2.
  // remove this once a newer version is available.
  implicit val cogenBigInt: Cogen[BigInt] =
    Cogen[Long].contramap(_.toLong)

  // this instance is not available in scalacheck 1.13.2.
  // remove this once a newer version is available.
  implicit val cogenBigDecimal: Cogen[BigDecimal] =
    Cogen[Double].contramap(_.toDouble)

  implicit val cogenSymbol: Cogen[Symbol] =
    Cogen[String].contramap(_.name)

  implicit val cogenUUID: Cogen[UUID] =
    Cogen[(Long, Long)].contramap(u => (u.getMostSignificantBits, u.getLeastSignificantBits))

  implicit val cogenDuration: Cogen[Duration] =
    Cogen[Long].contramap { d =>
      if (d == Duration.Inf) 3896691548866406746L
      else if (d == Duration.MinusInf) 1844151880988859955L
      else if (d == Duration.Undefined) -7917359255778781894L
      else d.length * (d.unit match {
        case DAYS => -6307593037248227856L
        case HOURS => -3527447467459552709L
        case MINUTES => 5955657079535371609L
        case SECONDS => 5314272869665647192L
        case MILLISECONDS => -2025740217814855607L
        case MICROSECONDS => -2965853209268633779L
        case NANOSECONDS => 6128745701389500153L
      })
    }
}

class LawTests extends FunSuite with Discipline {

  import KernelCheck._

  // The scalacheck defaults (100,100) are too high for scala-js.
  final val PropMaxSize: PosZInt = if (Platform.isJs) 10 else 100
  final val PropMinSuccessful: PosInt = if (Platform.isJs) 10 else 100

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = PropMinSuccessful, sizeRange = PropMaxSize)


  {
    // needed for Cogen[Map[...]]
    implicit val ohe: Ordering[HasEq[Int]] = Ordering[Int].on(_.a)
    checkAll("Eq[Map[String, HasEq[Int]]]", EqLawTests[Map[String, HasEq[Int]]].eqv)
  }

  checkAll("Eq[List[HasEq[Int]]]", EqLawTests[List[HasEq[Int]]].eqv)
  checkAll("Eq[Option[HasEq[Int]]]", EqLawTests[Option[HasEq[Int]]].eqv)
  checkAll("Eq[Vector[HasEq[Int]]]", EqLawTests[Vector[HasEq[Int]]].eqv)
  checkAll("Eq[Stream[HasEq[Int]]]", EqLawTests[Stream[HasEq[Int]]].eqv)
  checkAll("Eq[Queue[HasEq[Int]]]", EqLawTests[Queue[HasEq[Int]]].eqv)

  checkAll("PartialOrder[Set[Int]]", PartialOrderLawTests[Set[Int]].partialOrder)
  checkAll("PartialOrder[Set[Int]].reverse", PartialOrderLawTests(PartialOrder[Set[Int]].reverse).partialOrder)
  checkAll("PartialOrder[Set[Int]].reverse.reverse", PartialOrderLawTests(PartialOrder[Set[Int]].reverse.reverse).partialOrder)
  checkAll("PartialOrder[Option[HasPartialOrder[Int]]]", PartialOrderLawTests[Option[HasPartialOrder[Int]]].partialOrder)
  checkAll("PartialOrder[List[HasPartialOrder[Int]]]", PartialOrderLawTests[List[HasPartialOrder[Int]]].partialOrder)
  checkAll("PartialOrder[Vector[HasPartialOrder[Int]]]", PartialOrderLawTests[Vector[HasPartialOrder[Int]]].partialOrder)
  checkAll("PartialOrder[Stream[HasPartialOrder[Int]]]", PartialOrderLawTests[Stream[HasPartialOrder[Int]]].partialOrder)
  checkAll("PartialOrder[Queue[HasPartialOrder[Int]]]", PartialOrderLawTests[Queue[HasPartialOrder[Int]]].partialOrder)
  checkAll("Semilattice.asMeetPartialOrder[Set[Int]]", PartialOrderLawTests(Semilattice.asMeetPartialOrder[Set[Int]]).partialOrder)
  checkAll("Semilattice.asJoinPartialOrder[Set[Int]]", PartialOrderLawTests(Semilattice.asJoinPartialOrder[Set[Int]]).partialOrder)

  checkAll("Order[Unit]", OrderLawTests[Unit].order)
  checkAll("Order[Boolean]", OrderLawTests[Boolean].order)
  checkAll("Order[String]", OrderLawTests[String].order)
  checkAll("Order[Symbol]", OrderLawTests[Symbol].order)
  checkAll("Order[Byte]", OrderLawTests[Byte].order)
  checkAll("Order[Short]", OrderLawTests[Short].order)
  checkAll("Order[Char]", OrderLawTests[Char].order)
  checkAll("Order[Int]", OrderLawTests[Int].order)
  checkAll("Order[Long]", OrderLawTests[Long].order)
  checkAll("PartialOrder[BitSet]", PartialOrderLawTests[BitSet].partialOrder)
  checkAll("Order[BigInt]", OrderLawTests[BigInt].order)
  checkAll("Order[Duration]", OrderLawTests[Duration].order)
  checkAll("Order[UUID]", OrderLawTests[UUID].order)
  checkAll("Order[List[Int]]", OrderLawTests[List[Int]]  .order)
  checkAll("Order[Option[String]]", OrderLawTests[Option[String]].order)
  checkAll("Order[List[String]", OrderLawTests[List[String]].order)
  checkAll("Order[Vector[Int]]", OrderLawTests[Vector[Int]].order)
  checkAll("Order[Stream[Int]]", OrderLawTests[Stream[Int]].order)
  checkAll("Order[Queue[Int]]", OrderLawTests[Queue[Int]].order)
  checkAll("fromOrdering[Int]", OrderLawTests(Order.fromOrdering[Int]).order)
  checkAll("Order[Int].reverse", OrderLawTests(Order[Int].reverse).order)
  checkAll("Order[Int].reverse.reverse", OrderLawTests(Order[Int].reverse.reverse).order)

  checkAll("Monoid[String]", MonoidLawTests[String].monoid)
  checkAll("Monoid[String]", SerializableTests.serializable(Monoid[String]))
  checkAll("Monoid[Option[Int]]", MonoidLawTests[Option[Int]].monoid)
  checkAll("Monoid[Option[Int]]", SerializableTests.serializable(Monoid[String]))
  checkAll("Monoid[Option[String]]", MonoidLawTests[Option[String]].monoid)
  checkAll("Monoid[Option[String]]", SerializableTests.serializable(Monoid[String]))
  checkAll("Monoid[List[Int]]", MonoidLawTests[List[Int]].monoid)
  checkAll("Monoid[List[Int]]", SerializableTests.serializable(Monoid[List[Int]]))
  checkAll("Monoid[Vector[Int]]", MonoidLawTests[Vector[Int]].monoid)
  checkAll("Monoid[Vector[Int]]", SerializableTests.serializable(Monoid[Vector[Int]]))
  checkAll("Monoid[Stream[Int]]", MonoidLawTests[Stream[Int]].monoid)
  checkAll("Monoid[Stream[Int]]", SerializableTests.serializable(Monoid[Stream[Int]]))
  checkAll("Monoid[List[String]]", MonoidLawTests[List[String]].monoid)
  checkAll("Monoid[List[String]]", SerializableTests.serializable(Monoid[List[String]]))
  checkAll("Monoid[Map[String, Int]]", MonoidLawTests[Map[String, Int]].monoid)
  checkAll("Monoid[Map[String, Int]]", SerializableTests.serializable(Monoid[Map[String, Int]]))
  checkAll("Monoid[Queue[Int]]", MonoidLawTests[Queue[Int]].monoid)
  checkAll("Monoid[Queue[Int]]", SerializableTests.serializable(Monoid[Queue[Int]]))

  checkAll("BoundedSemilattice[BitSet]", BoundedSemilatticeTests[BitSet].boundedSemilattice)
  checkAll("BoundedSemilattice[BitSet]", SerializableTests.serializable(BoundedSemilattice[BitSet]))
  checkAll("BoundedSemilattice[Set[Int]]", BoundedSemilatticeTests[Set[Int]].boundedSemilattice)
  checkAll("BoundedSemilattice[Set[Int]]", SerializableTests.serializable(BoundedSemilattice[Set[Int]]))

  checkAll("CommutativeGroup[Unit]", CommutativeGroupTests[Unit].commutativeGroup)
  checkAll("CommutativeGroup[Unit]", SerializableTests.serializable(CommutativeGroup[Unit]))
  checkAll("CommutativeGroup[Byte]", CommutativeGroupTests[Byte].commutativeGroup)
  checkAll("CommutativeGroup[Byte]", SerializableTests.serializable(CommutativeGroup[Byte]))
  checkAll("CommutativeGroup[Short]", CommutativeGroupTests[Short].commutativeGroup)
  checkAll("CommutativeGroup[Short]", SerializableTests.serializable(CommutativeGroup[Short]))
  checkAll("CommutativeGroup[Int]", CommutativeGroupTests[Int].commutativeGroup)
  checkAll("CommutativeGroup[Int]", SerializableTests.serializable(CommutativeGroup[Int]))
  checkAll("CommutativeGroup[Long]", CommutativeGroupTests[Long].commutativeGroup)
  checkAll("CommutativeGroup[Long]", SerializableTests.serializable(CommutativeGroup[Long]))
  //checkAll("CommutativeGroup[Float]", CommutativeGroupTests[Float].commutativeGroup) // approximately associative
  //checkAll("CommutativeGroup[Double]", CommutativeGroupTests[Double].commutativeGroup) // approximately associative
  checkAll("CommutativeGroup[BigInt]", CommutativeGroupTests[BigInt].commutativeGroup)
  checkAll("CommutativeGroup[BigInt]", SerializableTests.serializable(CommutativeGroup[BigInt]))
  checkAll("CommutativeGroup[Duration]", CommutativeGroupTests[Duration].commutativeGroup)
  checkAll("CommutativeGroup[Duration]", SerializableTests.serializable(CommutativeGroup[Duration]))

  {
    // default Arbitrary[BigDecimal] is a bit too intense :/
    implicit val arbBigDecimal: Arbitrary[BigDecimal] =
      Arbitrary(arbitrary[Double].map(n => BigDecimal(n.toString)))
    checkAll("Order[BigDecimal]", OrderLawTests[BigDecimal].order)
    checkAll("CommutativeGroup[BigDecimal]", CommutativeGroupTests[BigDecimal].commutativeGroup)
    checkAll("CommutativeGroup[BigDecimal]", SerializableTests.serializable(CommutativeGroup[BigDecimal]))
  }

  checkAll("Band[(Int, Int)]", BandTests[(Int, Int)].band)
  checkAll("Band[(Int, Int)]", SerializableTests.serializable(Band[(Int, Int)]))

  checkAll("BoundedSemilattice[Unit]", BoundedSemilatticeTests[Unit].boundedSemilattice)
  checkAll("BoundedSemilattice[Unit]", SerializableTests.serializable(BoundedSemilattice[Unit]))

  // Comparison related

  // Something that can give NaN for test
  def subsetPartialOrder[A]: PartialOrder[Set[A]] = new PartialOrder[Set[A]] {
    def partialCompare(x: Set[A], y: Set[A]): Double =
      if (x == y) 0.0
      else if (x subsetOf y) -1.0
      else if (y subsetOf x) 1.0
      else Double.NaN
  }

  checkAll("subsetPartialOrder[Int]", PartialOrderLawTests(subsetPartialOrder[Int]).partialOrder)

  implicit val arbitraryComparison: Arbitrary[Comparison] =
    Arbitrary(Gen.oneOf(Comparison.GreaterThan, Comparison.EqualTo, Comparison.LessThan))

  implicit val cogenComparison: Cogen[Comparison] =
    Cogen[Int].contramap(_.toInt)

  checkAll("Eq[Comparison]", EqLawTests[Comparison].eqv)

  test("comparison") {
    val order = Order[Int]
    val eqv = Eq[Comparison]
    eqv.eqv(order.comparison(1, 0),  Comparison.GreaterThan) &&
    eqv.eqv(order.comparison(0, 0),  Comparison.EqualTo)     &&
    eqv.eqv(order.comparison(-1, 0), Comparison.LessThan)
  }

  test("partialComparison") {
    val po = subsetPartialOrder[Int]
    val eqv = Eq[Option[Comparison]]
    eqv.eqv(po.partialComparison(Set(1), Set()),        Some(Comparison.GreaterThan)) &&
    eqv.eqv(po.partialComparison(Set(), Set()),         Some(Comparison.EqualTo))     &&
    eqv.eqv(po.partialComparison(Set(), Set(1)),        Some(Comparison.LessThan))    &&
    eqv.eqv(po.partialComparison(Set(1, 2), Set(2, 3)), None)
  }

  test("signum . toInt . comparison = signum . compare") {
    check { (i: Int, j: Int) =>
      val found = Order[Int].comparison(i, j)
      val expected = Order[Int].compare(i, j)
      Eq[Int].eqv(found.toInt.signum, expected.signum)
    }
  }

  test("signum . toDouble . partialComparison = signum . partialCompare") {
    check { (x: Set[Int], y: Set[Int]) =>
      val found = subsetPartialOrder[Int].partialComparison(x, y).map(_.toDouble.signum)
      val expected = Some(subsetPartialOrder[Int].partialCompare(x, y)).filter(d => !d.isNaN).map(_.signum)
      Eq[Option[Int]].eqv(found, expected)
    }
  }

  // esoteric machinery follows...

  implicit lazy val band: Band[(Int, Int)] =
    new Band[(Int, Int)] {
      def combine(a: (Int, Int), b: (Int, Int)) = (a._1, b._2)
    }

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
    implicit val cogNOrder: Cogen[Order[N]] =
      Cogen[Unit].contramap(_ => ())
    // The arbitrary `Eq[N]` values are created by mapping N values to random
    // integers.
    implicit val arbNEq: Arbitrary[Eq[N]] = Arbitrary(arbitrary[Int].map { seed =>
      val mapping = new Random(seed).shuffle(Vector.range(0, nMax))
      Eq.by { (n: N) => mapping(n.n) }
    })
    implicit val cogNEq: Cogen[Eq[N]] =
      Cogen[Unit].contramap(_ => ())
    // needed because currently we don't have Vector instances
    implicit val vectorNEq: Eq[Vector[N]] = Eq.fromUniversalEquals
    // The `Eq[Order[N]]` instance enumerates all possible `N` values in a
    // `Vector` and considers two `Order[N]` instances to be equal if they
    // result in the same sorting of that vector.
    implicit val NOrderEq: Eq[Order[N]] = Eq.by { order: Order[N] =>
      Vector.tabulate(nMax)(N).sorted(order.toOrdering)
    }
    implicit val NEqEq: Eq[Eq[N]] = new Eq[Eq[N]] {
      def eqv(a: Eq[N], b: Eq[N]) =
        Iterator.tabulate(nMax)(N)
          .flatMap { x => Iterator.tabulate(nMax)(N).map((x, _)) }
          .forall { case (x, y) => a.eqv(x, y) == b.eqv(x, y) }
    }

    implicit val monoidOrderN = Order.whenEqualMonoid[N]
    checkAll("Monoid[Order[N]]", MonoidLawTests[Order[N]].monoid)
    checkAll("Band[Order[N]]", BandTests[Order[N]].band)

    {
      implicit val bsEqN: BoundedSemilattice[Eq[N]] = Eq.allEqualBoundedSemilattice[N]
      checkAll("BoundedSemilattice[Eq[N]]", BoundedSemilatticeTests[Eq[N]].boundedSemilattice)
    }
    {
      implicit val sEqN: Semilattice[Eq[N]] = Eq.anyEqualSemilattice[N]
      checkAll("Semilattice[Eq[N]]", SemilatticeTests[Eq[N]].semilattice)
    }
  }

  case class HasEq[A](a: A)

  object HasEq {
    implicit def hasEq[A: Eq]: Eq[HasEq[A]] =
      Eq[A].on(_.a)
    implicit def hasEqArbitrary[A: Arbitrary]: Arbitrary[HasEq[A]] =
      Arbitrary(arbitrary[A].map(HasEq(_)))
    implicit def hasCogen[A: Cogen]: Cogen[HasEq[A]] =
      Cogen[A].contramap(_.a)
  }

  case class HasPartialOrder[A](a: A)

  object HasPartialOrder {
    implicit def hasPartialOrder[A: PartialOrder]: PartialOrder[HasPartialOrder[A]] =
      PartialOrder[A].on(_.a)
    implicit def hasPartialOrderArbitrary[A: Arbitrary]: Arbitrary[HasPartialOrder[A]] =
      Arbitrary(arbitrary[A].map(HasPartialOrder(_)))
    implicit def hasCogen[A: Cogen]: Cogen[HasPartialOrder[A]] =
      Cogen[A].contramap(_.a)
  }

  case class LawChecker[L <: Laws](name: String, laws: L) {
    def check(f: L => L#RuleSet): Unit = checkAll(name, f(laws))
  }

  private[laws] def laws[L[_] <: Laws, A](implicit lws: L[A], tag: TypeTagM[A]): LawChecker[L[A]] =
    laws[L, A]("")

  private[laws] def laws[L[_] <: Laws, A](extraTag: String)(implicit laws: L[A], tag: TypeTagM[A]): LawChecker[L[A]] =
    LawChecker("[" + tag.name.toString + (if(extraTag != "") "@@" + extraTag else "") + "]", laws)
}
