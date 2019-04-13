package cats.kernel
package laws

import cats.kernel.instances.all._
import cats.kernel.laws.discipline._
import cats.platform.Platform

import org.typelevel.discipline.scalatest.Discipline
import org.scalacheck.{Arbitrary, Cogen, Gen}
import Arbitrary.arbitrary
import org.scalactic.anyvals.{PosInt, PosZInt}
import org.scalatest.FunSuite

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.collection.immutable.{BitSet, Queue}
import scala.util.Random

import java.util.UUID
import java.util.concurrent.TimeUnit.{DAYS, HOURS, MICROSECONDS, MILLISECONDS, MINUTES, NANOSECONDS, SECONDS}

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
    Arbitrary(
      Gen.oneOf(
        Gen.choose(-n, n).map(Duration(_, DAYS)),
        Gen.choose(-n * 24L, n * 24L).map(Duration(_, HOURS)),
        Gen.choose(-n * 1440L, n * 1440L).map(Duration(_, MINUTES)),
        Gen.choose(-n * 86400L, n * 86400L).map(Duration(_, SECONDS)),
        Gen.choose(-n * 86400000L, n * 86400000L).map(Duration(_, MILLISECONDS)),
        Gen.choose(-n * 86400000000L, n * 86400000000L).map(Duration(_, MICROSECONDS)),
        Gen.choose(-n * 86400000000000L, n * 86400000000000L).map(Duration(_, NANOSECONDS))
      )
    )
  }

  implicit val arbitraryFiniteDuration: Arbitrary[FiniteDuration] = {
    // max range is +/- 292 years, but we give ourselves some extra headroom
    // to ensure that we can add these things up. they crash on overflow.
    val n = (292L * 365) / 50
    Arbitrary(
      Gen.oneOf(
        Gen.choose(-n, n).map(FiniteDuration(_, DAYS)),
        Gen.choose(-n * 24L, n * 24L).map(FiniteDuration(_, HOURS)),
        Gen.choose(-n * 1440L, n * 1440L).map(FiniteDuration(_, MINUTES)),
        Gen.choose(-n * 86400L, n * 86400L).map(FiniteDuration(_, SECONDS)),
        Gen.choose(-n * 86400000L, n * 86400000L).map(FiniteDuration(_, MILLISECONDS)),
        Gen.choose(-n * 86400000000L, n * 86400000000L).map(FiniteDuration(_, MICROSECONDS)),
        Gen.choose(-n * 86400000000000L, n * 86400000000000L).map(FiniteDuration(_, NANOSECONDS))
      )
    )
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
      else
        d.length * (d.unit match {
          case DAYS         => -6307593037248227856L
          case HOURS        => -3527447467459552709L
          case MINUTES      => 5955657079535371609L
          case SECONDS      => 5314272869665647192L
          case MILLISECONDS => -2025740217814855607L
          case MICROSECONDS => -2965853209268633779L
          case NANOSECONDS  => 6128745701389500153L
        })
    }

  implicit val cogenFiniteDuration: Cogen[FiniteDuration] =
    Cogen[Long].contramap { d =>
      d.length * (d.unit match {
        case DAYS         => -6307593037248227856L
        case HOURS        => -3527447467459552709L
        case MINUTES      => 5955657079535371609L
        case SECONDS      => 5314272869665647192L
        case MILLISECONDS => -2025740217814855607L
        case MICROSECONDS => -2965853209268633779L
        case NANOSECONDS  => 6128745701389500153L
      })
    }
}

class Tests extends FunSuite with Discipline {

  import KernelCheck._

  // The scalacheck defaults (100,100) are too high for scala-js.
  final val PropMaxSize: PosZInt = if (Platform.isJs) 10 else 100
  final val PropMinSuccessful: PosInt = if (Platform.isJs) 10 else 100
  final val PropWorkers: PosInt = if (Platform.isJvm) PosInt(2) else PosInt(1)

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = PropMinSuccessful, sizeRange = PropMaxSize, workers = PropWorkers)

  {
    // needed for Cogen[Map[...]]
    implicit val ohe: Ordering[HasEq[Int]] = Ordering.by[HasEq[Int], Int](_.a)
    checkAll("Eq[Map[String, HasEq[Int]]]", EqTests[Map[String, HasEq[Int]]].eqv)
  }

  checkAll("Eq[List[HasEq[Int]]]", EqTests[List[HasEq[Int]]].eqv)
  checkAll("Eq[Option[HasEq[Int]]]", EqTests[Option[HasEq[Int]]].eqv)
  checkAll("Eq[Vector[HasEq[Int]]]", EqTests[Vector[HasEq[Int]]].eqv)
  checkAll("Eq[Stream[HasEq[Int]]]", EqTests[Stream[HasEq[Int]]].eqv)
  checkAll("Eq[Queue[HasEq[Int]]]", EqTests[Queue[HasEq[Int]]].eqv)

  checkAll("PartialOrder[Set[Int]]", PartialOrderTests[Set[Int]].partialOrder)
  checkAll("PartialOrder.reverse(PartialOrder[Set[Int]])",
           PartialOrderTests(PartialOrder.reverse(PartialOrder[Set[Int]])).partialOrder)
  checkAll(
    "PartialOrder.reverse(PartialOrder.reverse(PartialOrder[Set[Int]]))",
    PartialOrderTests(PartialOrder.reverse(PartialOrder.reverse(PartialOrder[Set[Int]]))).partialOrder
  )
  checkAll("PartialOrder[Option[HasPartialOrder[Int]]]", PartialOrderTests[Option[HasPartialOrder[Int]]].partialOrder)
  checkAll("PartialOrder[List[HasPartialOrder[Int]]]", PartialOrderTests[List[HasPartialOrder[Int]]].partialOrder)
  checkAll("PartialOrder[Vector[HasPartialOrder[Int]]]", PartialOrderTests[Vector[HasPartialOrder[Int]]].partialOrder)
  checkAll("PartialOrder[Stream[HasPartialOrder[Int]]]", PartialOrderTests[Stream[HasPartialOrder[Int]]].partialOrder)
  checkAll("PartialOrder[Queue[HasPartialOrder[Int]]]", PartialOrderTests[Queue[HasPartialOrder[Int]]].partialOrder)
  checkAll("Semilattice.asMeetPartialOrder[Set[Int]]",
           PartialOrderTests(Semilattice.asMeetPartialOrder[Set[Int]]).partialOrder)
  checkAll("Semilattice.asJoinPartialOrder[Set[Int]]",
           PartialOrderTests(Semilattice.asJoinPartialOrder[Set[Int]]).partialOrder)

  checkAll("Order[Unit]", OrderTests[Unit].order)
  checkAll("Order[Boolean]", OrderTests[Boolean].order)
  checkAll("Order[String]", OrderTests[String].order)
  checkAll("Order[Symbol]", OrderTests[Symbol].order)
  checkAll("Order[Byte]", OrderTests[Byte].order)
  checkAll("Order[Short]", OrderTests[Short].order)
  checkAll("Order[Char]", OrderTests[Char].order)
  checkAll("Order[Int]", OrderTests[Int].order)
  checkAll("Order[Long]", OrderTests[Long].order)
  checkAll("PartialOrder[BitSet]", PartialOrderTests[BitSet].partialOrder)
  checkAll("Order[BigInt]", OrderTests[BigInt].order)
  checkAll("Order[Duration]", OrderTests[Duration].order)
  checkAll("Order[FiniteDuration]", OrderTests[FiniteDuration].order)
  checkAll("Order[UUID]", OrderTests[UUID].order)
  checkAll("Order[List[Int]]", OrderTests[List[Int]].order)
  checkAll("Order[Option[String]]", OrderTests[Option[String]].order)
  checkAll("Order[List[String]", OrderTests[List[String]].order)
  checkAll("Order[Vector[Int]]", OrderTests[Vector[Int]].order)
  checkAll("Order[Stream[Int]]", OrderTests[Stream[Int]].order)
  checkAll("Order[Queue[Int]]", OrderTests[Queue[Int]].order)
  checkAll("fromOrdering[Int]", OrderTests(Order.fromOrdering[Int]).order)
  checkAll("Order.reverse(Order[Int])", OrderTests(Order.reverse(Order[Int])).order)
  checkAll("Order.reverse(Order.reverse(Order[Int]))", OrderTests(Order.reverse(Order.reverse(Order[Int]))).order)
  checkAll("Order.fromLessThan[Int](_ < _)", OrderTests(Order.fromLessThan[Int](_ < _)).order)

  checkAll("Monoid[String]", MonoidTests[String].monoid)
  checkAll("Monoid[String]", SerializableTests.serializable(Monoid[String]))
  checkAll("Monoid[Option[Int]]", MonoidTests[Option[Int]].monoid)
  checkAll("Monoid[Option[Int]]", SerializableTests.serializable(Monoid[String]))
  checkAll("Monoid[Option[String]]", MonoidTests[Option[String]].monoid)
  checkAll("Monoid[Option[String]]", SerializableTests.serializable(Monoid[String]))
  checkAll("Monoid[List[Int]]", MonoidTests[List[Int]].monoid)
  checkAll("Monoid[List[Int]]", SerializableTests.serializable(Monoid[List[Int]]))
  checkAll("Monoid[Vector[Int]]", MonoidTests[Vector[Int]].monoid)
  checkAll("Monoid[Vector[Int]]", SerializableTests.serializable(Monoid[Vector[Int]]))
  checkAll("Monoid[Stream[Int]]", MonoidTests[Stream[Int]].monoid)
  checkAll("Monoid[Stream[Int]]", SerializableTests.serializable(Monoid[Stream[Int]]))
  checkAll("Monoid[List[String]]", MonoidTests[List[String]].monoid)
  checkAll("Monoid[List[String]]", SerializableTests.serializable(Monoid[List[String]]))
  checkAll("Monoid[Map[String, String]]", MonoidTests[Map[String, String]].monoid)
  checkAll("Monoid[Map[String, String]]", SerializableTests.serializable(Monoid[Map[String, String]]))
  checkAll("Monoid[Queue[Int]]", MonoidTests[Queue[Int]].monoid)
  checkAll("Monoid[Queue[Int]]", SerializableTests.serializable(Monoid[Queue[Int]]))

  checkAll("CommutativeMonoid[Map[String, Int]]", CommutativeMonoidTests[Map[String, Int]].commutativeMonoid)
  checkAll("CommutativeMonoid[Map[String, Int]]", SerializableTests.serializable(CommutativeMonoid[Map[String, Int]]))

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
  // checkAll("CommutativeGroup[Float]", CommutativeGroupTests[Float].commutativeGroup) // approximately associative
  // checkAll("CommutativeGroup[Double]", CommutativeGroupTests[Double].commutativeGroup) // approximately associative
  checkAll("CommutativeGroup[BigInt]", CommutativeGroupTests[BigInt].commutativeGroup)
  checkAll("CommutativeGroup[BigInt]", SerializableTests.serializable(CommutativeGroup[BigInt]))
  checkAll("CommutativeGroup[Duration]", CommutativeGroupTests[Duration].commutativeGroup)
  checkAll("CommutativeGroup[Duration]", SerializableTests.serializable(CommutativeGroup[Duration]))
  checkAll("CommutativeGroup[FiniteDuration]", CommutativeGroupTests[FiniteDuration].commutativeGroup)
  checkAll("CommutativeGroup[FiniteDuration]", SerializableTests.serializable(CommutativeGroup[FiniteDuration]))

  checkAll("Hash[Unit]", HashTests[Unit].hash)
  checkAll("Hash[Boolean]", HashTests[Boolean].hash)
  checkAll("Hash[String]", HashTests[String].hash)
  checkAll("Hash[Symbol]", HashTests[Symbol].hash)
  checkAll("Hash[Byte]", HashTests[Byte].hash)
  checkAll("Hash[Short]", HashTests[Short].hash)
  checkAll("Hash[Char]", HashTests[Char].hash)
  checkAll("Hash[Int]", HashTests[Int].hash)
  checkAll("Hash[Duration]", HashTests[Duration].hash)
  checkAll("Hash[FiniteDuration]", HashTests[FiniteDuration].hash)

  // NOTE: Do not test for Float/Double/Long. These types'
  // `##` is different from `hashCode`. See [[scala.runtime.Statics.anyHash]].
  // checkAll("Hash[Float]" , HashTests[Float].hash)
  // checkAll("Hash[Double]" , HashTests[Double].hash)
  checkAll("Hash[BitSet]", HashTests[BitSet].hash)
  checkAll("Hash[BigDecimal]", HashTests[BigDecimal].hash)
  checkAll("Hash[BigInt]", HashTests[BigInt].hash)
  checkAll("Hash[UUID]", HashTests[UUID].hash)
  checkAll("Hash[List[Int]]", HashTests[List[Int]].hash)
  checkAll("Hash[Option[String]]", HashTests[Option[String]].hash)
  checkAll("Hash[List[String]]", HashTests[List[String]].hash)
  checkAll("Hash[Vector[Int]]", HashTests[Vector[Int]].hash)
  checkAll("Hash[Stream[Int]]", HashTests[Stream[Int]].hash)
  checkAll("Hash[Set[Int]]", HashTests[Set[Int]].hash)
  checkAll("Hash[(Int, String)]", HashTests[(Int, String)].hash)
  checkAll("Hash[Either[Int, String]]", HashTests[Either[Int, String]].hash)
  checkAll("Hash[Map[Int, String]]", HashTests[Map[Int, String]].hash)
  checkAll("Hash[Queue[Int]", HashTests[Queue[Int]].hash)

  {
    // default Arbitrary[BigDecimal] is a bit too intense :/
    implicit val arbBigDecimal: Arbitrary[BigDecimal] =
      Arbitrary(arbitrary[Double].map(n => BigDecimal(n.toString)))
    checkAll("Order[BigDecimal]", OrderTests[BigDecimal].order)
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
      else if (x.subsetOf(y)) -1.0
      else if (y.subsetOf(x)) 1.0
      else Double.NaN
  }

  checkAll("subsetPartialOrder[Int]", PartialOrderTests(subsetPartialOrder[Int]).partialOrder)

  {
    implicit def subsetPartialOrdering[A]: PartialOrdering[Set[A]] = new PartialOrdering[Set[A]] {

      override def tryCompare(x: Set[A], y: Set[A]): Option[Int] =
        if (x == y) Some(0)
        else if (x.subsetOf(y)) Some(-1)
        else if (y.subsetOf(x)) Some(1)
        else None

      override def lteq(x: Set[A], y: Set[A]): Boolean = (x.subsetOf(y)) || (x == y)
    }
    checkAll("fromPartialOrdering[Int]", PartialOrderTests(PartialOrder.fromPartialOrdering[Set[Int]]).partialOrder)
  }

  implicit val arbitraryComparison: Arbitrary[Comparison] =
    Arbitrary(Gen.oneOf(Comparison.GreaterThan, Comparison.EqualTo, Comparison.LessThan))

  implicit val cogenComparison: Cogen[Comparison] =
    Cogen[Int].contramap(_.toInt)

  checkAll("Eq[Comparison]", EqTests[Comparison].eqv)

  test("comparison") {
    val order = Order[Int]
    val eqv = Eq[Comparison]
    eqv.eqv(order.comparison(1, 0), Comparison.GreaterThan) &&
    eqv.eqv(order.comparison(0, 0), Comparison.EqualTo) &&
    eqv.eqv(order.comparison(-1, 0), Comparison.LessThan)
  }

  test("partialComparison") {
    val po = subsetPartialOrder[Int]
    val eqv = Eq[Option[Comparison]]
    eqv.eqv(po.partialComparison(Set(1), Set()), Some(Comparison.GreaterThan)) &&
    eqv.eqv(po.partialComparison(Set(), Set()), Some(Comparison.EqualTo)) &&
    eqv.eqv(po.partialComparison(Set(), Set(1)), Some(Comparison.LessThan)) &&
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
      Order.by { (n: N) =>
        order(n.n)
      }
    })
    implicit val cogNOrder: Cogen[Order[N]] =
      Cogen[Unit].contramap(_ => ())
    // The arbitrary `Eq[N]` values are created by mapping N values to random
    // integers.
    implicit val arbNEq: Arbitrary[Eq[N]] = Arbitrary(arbitrary[Int].map { seed =>
      val mapping = new Random(seed).shuffle(Vector.range(0, nMax))
      Eq.by { (n: N) =>
        mapping(n.n)
      }
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
        Iterator
          .tabulate(nMax)(N)
          .flatMap { x =>
            Iterator.tabulate(nMax)(N).map((x, _))
          }
          .forall { case (x, y) => a.eqv(x, y) == b.eqv(x, y) }
    }

    implicit val monoidOrderN = Order.whenEqualMonoid[N]
    checkAll("Monoid[Order[N]]", MonoidTests[Order[N]].monoid)
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
      Eq.by(_.a)
    implicit def hasEqArbitrary[A: Arbitrary]: Arbitrary[HasEq[A]] =
      Arbitrary(arbitrary[A].map(HasEq(_)))
    implicit def hasCogen[A: Cogen]: Cogen[HasEq[A]] =
      Cogen[A].contramap(_.a)
  }

  case class HasPartialOrder[A](a: A)

  object HasPartialOrder {
    implicit def hasPartialOrder[A: PartialOrder]: PartialOrder[HasPartialOrder[A]] =
      PartialOrder.by(_.a)
    implicit def hasPartialOrderArbitrary[A: Arbitrary]: Arbitrary[HasPartialOrder[A]] =
      Arbitrary(arbitrary[A].map(HasPartialOrder(_)))
    implicit def hasCogen[A: Cogen]: Cogen[HasPartialOrder[A]] =
      Cogen[A].contramap(_.a)
  }

  case class HasHash[A](a: A)

  object HasHash {
    implicit def hasHash[A: Hash]: Hash[HasHash[A]] =
      Hash.by(_.a)
    implicit def hasHashArbitrary[A: Arbitrary]: Arbitrary[HasHash[A]] =
      Arbitrary(arbitrary[A].map(HasHash(_)))
    implicit def hasCogen[A: Cogen]: Cogen[HasHash[A]] =
      Cogen[A].contramap(_.a)
  }
}
