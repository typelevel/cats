package cats.kernel
package laws

import catalysts.Platform
import catalysts.macros.TypeTagM

import cats.kernel.instances.all._


import org.typelevel.discipline.{ Laws }
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

class LawTests extends FunSuite with Discipline {

  // The scalacheck defaults (100,100) are too high for scala-js.
  final val PropMaxSize: PosZInt = if (Platform.isJs) 10 else 100
  final val PropMinSuccessful: PosInt = if (Platform.isJs) 10 else 100

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = PropMinSuccessful, sizeRange = PropMaxSize)

  implicit def hashLaws[A: Cogen: Eq: Arbitrary]: HashLaws[A] = HashLaws[A]

  implicit def orderLaws[A: Cogen: Eq: Arbitrary]: OrderLaws[A] = OrderLaws[A]
  implicit def groupLaws[A: Cogen: Eq: Arbitrary]: GroupLaws[A] = GroupLaws[A]

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

  {
    // needed for Cogen[Map[...]]
    implicit val ohe: Ordering[HasEq[Int]] = Ordering[Int].on(_.a)
    laws[OrderLaws, Map[String, HasEq[Int]]].check(_.eqv)
  }

  laws[HashLaws, Unit].check(_.hash)
  laws[HashLaws, Boolean].check(_.hash)
  laws[HashLaws, String].check(_.hash)
  laws[HashLaws, Symbol].check(_.hash)
  laws[HashLaws, Byte].check(_.hash)
  laws[HashLaws, Short].check(_.hash)
  laws[HashLaws, Char].check(_.hash)
  laws[HashLaws, Int].check(_.hash)
  laws[HashLaws, Float].check(_.hash)
  laws[HashLaws, Double].check(_.hash)
  laws[HashLaws, Long].check(_.hash)
  laws[HashLaws, BitSet].check(_.hash)
  laws[HashLaws, BigDecimal].check(_.hash)
  laws[HashLaws, BigInt].check(_.hash)
  laws[HashLaws, UUID].check(_.hash)
  laws[HashLaws, Duration].check(_.hash)
  laws[HashLaws, List[Int]].check(_.hash)
  laws[HashLaws, Option[String]].check(_.hash)
  laws[HashLaws, List[String]].check(_.hash)
  laws[HashLaws, Vector[Int]].check(_.hash)
  laws[HashLaws, Queue[Int]].check(_.hash)
  laws[HashLaws, Stream[Int]].check(_.hash)
  laws[HashLaws, Set[Int]].check(_.hash)
  laws[HashLaws, (Int, String)].check(_.hash)
  laws[HashLaws, Either[Int, String]].check(_.hash)
  laws[HashLaws, Map[Int, String]].check(_.hash)

  laws[HashLaws, Unit].check(_.sameAsUniversalHash)
  laws[HashLaws, Boolean].check(_.sameAsUniversalHash)
  laws[HashLaws, String].check(_.sameAsUniversalHash)
  laws[HashLaws, Symbol].check(_.sameAsUniversalHash)
  laws[HashLaws, Byte].check(_.sameAsUniversalHash)
  laws[HashLaws, Short].check(_.sameAsUniversalHash)
  laws[HashLaws, Char].check(_.sameAsUniversalHash)
  laws[HashLaws, Int].check(_.sameAsUniversalHash)
  laws[HashLaws, Float].check(_.sameAsUniversalHash)
  laws[HashLaws, Double].check(_.sameAsUniversalHash)
  laws[HashLaws, Long].check(_.sameAsUniversalHash)
  laws[HashLaws, BitSet].check(_.sameAsUniversalHash)
  laws[HashLaws, BigDecimal].check(_.sameAsUniversalHash)
  laws[HashLaws, BigInt].check(_.sameAsUniversalHash)
  laws[HashLaws, UUID].check(_.sameAsUniversalHash)
  laws[HashLaws, List[Int]].check(_.sameAsUniversalHash)
  laws[HashLaws, Option[String]].check(_.sameAsUniversalHash)
  laws[HashLaws, List[String]].check(_.sameAsUniversalHash)
  laws[HashLaws, Queue[Int]].check(_.sameAsUniversalHash)
  laws[HashLaws, Vector[Int]].check(_.sameAsUniversalHash)
  laws[HashLaws, Stream[Int]].check(_.sameAsUniversalHash)
  laws[HashLaws, Set[Int]].check(_.sameAsUniversalHash)
  laws[HashLaws, (Int, String)].check(_.sameAsUniversalHash)
  laws[HashLaws, Either[Int, String]].check(_.sameAsUniversalHash)
  laws[HashLaws, Map[Int, String]].check(_.sameAsUniversalHash)

  // NOTE: Do not test for Float/Double/Long. These types'
  // `##` is different from `hashCode`. See [[scala.runtime.Statics.anyHash]].
  laws[HashLaws, Unit].check(_.sameAsScalaHashing)
  laws[HashLaws, Boolean].check(_.sameAsScalaHashing)
  laws[HashLaws, String].check(_.sameAsScalaHashing)
  laws[HashLaws, Symbol].check(_.sameAsScalaHashing)
  laws[HashLaws, Byte].check(_.sameAsScalaHashing)
  laws[HashLaws, Short].check(_.sameAsScalaHashing)
  laws[HashLaws, Char].check(_.sameAsScalaHashing)
  laws[HashLaws, Int].check(_.sameAsScalaHashing)
  laws[HashLaws, BitSet].check(_.sameAsScalaHashing)
  laws[HashLaws, BigDecimal].check(_.sameAsScalaHashing)
  laws[HashLaws, BigInt].check(_.sameAsScalaHashing)
  laws[HashLaws, UUID].check(_.sameAsScalaHashing)
  laws[HashLaws, Duration].check(_.sameAsScalaHashing)

  laws[HashLaws, Option[HasHash[Int]]].check(_.hash)
  laws[HashLaws, List[HasHash[Int]]].check(_.hash)
  laws[HashLaws, Vector[HasHash[Int]]].check(_.hash)
  laws[HashLaws, Stream[HasHash[Int]]].check(_.hash)
  laws[HashLaws, Queue[HasHash[Int]]].check(_.hash)

  laws[OrderLaws, List[HasEq[Int]]].check(_.eqv)
  laws[OrderLaws, Option[HasEq[Int]]].check(_.eqv)
  laws[OrderLaws, Vector[HasEq[Int]]].check(_.eqv)
  laws[OrderLaws, Stream[HasEq[Int]]].check(_.eqv)
  laws[OrderLaws, Queue[HasEq[Int]]].check(_.eqv)

  laws[OrderLaws, Set[Int]].check(_.partialOrder)
  laws[OrderLaws, Set[Int]]("reverse").check(_.partialOrder(PartialOrder[Set[Int]].reverse))
  laws[OrderLaws, Set[Int]]("reverse.reverse").check(_.partialOrder(PartialOrder[Set[Int]].reverse.reverse))
  laws[OrderLaws, Option[HasPartialOrder[Int]]].check(_.partialOrder)
  laws[OrderLaws, List[HasPartialOrder[Int]]].check(_.partialOrder)
  laws[OrderLaws, Vector[HasPartialOrder[Int]]].check(_.partialOrder)
  laws[OrderLaws, Stream[HasPartialOrder[Int]]].check(_.partialOrder)
  laws[OrderLaws, Queue[HasPartialOrder[Int]]].check(_.partialOrder)
  laws[OrderLaws, Set[Int]]("asMeetPartialOrder").check(_.partialOrder(Semilattice.asMeetPartialOrder[Set[Int]]))
  laws[OrderLaws, Set[Int]]("asJoinPartialOrder").check(_.partialOrder(Semilattice.asJoinPartialOrder[Set[Int]]))

  laws[OrderLaws, Unit].check(_.order)
  laws[OrderLaws, Boolean].check(_.order)
  laws[OrderLaws, String].check(_.order)
  laws[OrderLaws, Symbol].check(_.order)
  laws[OrderLaws, Byte].check(_.order)
  laws[OrderLaws, Short].check(_.order)
  laws[OrderLaws, Char].check(_.order)
  laws[OrderLaws, Int].check(_.order)
  laws[OrderLaws, Long].check(_.order)
  laws[OrderLaws, BitSet].check(_.partialOrder)
  laws[OrderLaws, BigInt].check(_.order)
  laws[OrderLaws, Duration].check(_.order)
  laws[OrderLaws, UUID].check(_.order)
  laws[OrderLaws, List[Int]].check(_.order)
  laws[OrderLaws, Option[String]].check(_.order)
  laws[OrderLaws, List[String]].check(_.order)
  laws[OrderLaws, Vector[Int]].check(_.order)
  laws[OrderLaws, Stream[Int]].check(_.order)
  laws[OrderLaws, Queue[Int]].check(_.order)
  laws[OrderLaws, Int]("fromOrdering").check(_.order(Order.fromOrdering[Int]))
  laws[OrderLaws, Int]("reverse").check(_.order(Order[Int].reverse))
  laws[OrderLaws, Int]("reverse.reverse").check(_.order(Order[Int].reverse.reverse))

  laws[GroupLaws, String].check(_.monoid)
  laws[GroupLaws, Option[Int]].check(_.monoid)
  laws[GroupLaws, Option[String]].check(_.monoid)
  laws[GroupLaws, List[Int]].check(_.monoid)
  laws[GroupLaws, Vector[Int]].check(_.monoid)
  laws[GroupLaws, Stream[Int]].check(_.monoid)
  laws[GroupLaws, List[String]].check(_.monoid)
  laws[GroupLaws, Map[String, Int]].check(_.monoid)
  laws[GroupLaws, Queue[Int]].check(_.monoid)

  laws[GroupLaws, BitSet].check(_.boundedSemilattice)
  laws[GroupLaws, Set[Int]].check(_.boundedSemilattice)

  laws[GroupLaws, Unit].check(_.commutativeGroup)
  laws[GroupLaws, Byte].check(_.commutativeGroup)
  laws[GroupLaws, Short].check(_.commutativeGroup)
  laws[GroupLaws, Int].check(_.commutativeGroup)
  laws[GroupLaws, Long].check(_.commutativeGroup)
  //laws[GroupLaws, Float].check(_.commutativeGroup) // approximately associative
  //laws[GroupLaws, Double].check(_.commutativeGroup) // approximately associative
  laws[GroupLaws, BigInt].check(_.commutativeGroup)
  laws[GroupLaws, Duration].check(_.commutativeGroup)

  {
    // default Arbitrary[BigDecimal] is a bit too intense :/
    implicit val arbBigDecimal: Arbitrary[BigDecimal] =
      Arbitrary(arbitrary[Double].map(n => BigDecimal(n.toString)))
    laws[OrderLaws, BigDecimal].check(_.order)
    laws[GroupLaws, BigDecimal].check(_.commutativeGroup)
  }

  laws[GroupLaws, (Int, Int)].check(_.band)

  laws[GroupLaws, Unit].check(_.boundedSemilattice)

  // Comparison related

  // Something that can give NaN for test
  def subsetPartialOrder[A]: PartialOrder[Set[A]] = new PartialOrder[Set[A]] {
    def partialCompare(x: Set[A], y: Set[A]): Double =
      if (x == y) 0.0
      else if (x subsetOf y) -1.0
      else if (y subsetOf x) 1.0
      else Double.NaN
  }

  laws[OrderLaws, Set[Int]]("subset").check(_.partialOrder(subsetPartialOrder[Int]))

  implicit val arbitraryComparison: Arbitrary[Comparison] =
    Arbitrary(Gen.oneOf(Comparison.GreaterThan, Comparison.EqualTo, Comparison.LessThan))

  implicit val cogenComparison: Cogen[Comparison] =
    Cogen[Int].contramap(_.toInt)

  laws[OrderLaws, Comparison].check(_.eqv)

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
    laws[GroupLaws, Order[N]].check(_.monoid)
    laws[GroupLaws, Order[N]].check(_.band)

    {
      implicit val bsEqN: BoundedSemilattice[Eq[N]] = Eq.allEqualBoundedSemilattice[N]
      laws[GroupLaws, Eq[N]].check(_.boundedSemilattice)
    }
    {
      implicit val sEqN: Semilattice[Eq[N]] = Eq.anyEqualSemilattice[N]
      laws[GroupLaws, Eq[N]].check(_.semilattice)
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

  case class HasHash[A](a: A)

  object HasHash {
    implicit def hasHash[A: Hash]: Hash[HasHash[A]] =
      Hash.by(_.a) // not Hash[A].on(_.a) because of diamond inheritance problems with Eq
    implicit def hasHashArbitrary[A: Arbitrary]: Arbitrary[HasHash[A]] =
      Arbitrary(arbitrary[A].map(HasHash(_)))
    implicit def hasCogen[A: Cogen]: Cogen[HasHash[A]] =
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
