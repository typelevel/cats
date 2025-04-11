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

package cats.kernel
package laws

import cats.kernel.laws.discipline.*
import cats.platform.Platform
import munit.DisciplineSuite
import org.scalacheck.{Arbitrary, Cogen, Gen, Prop}
import Prop.forAll
import Arbitrary.arbitrary
import cats.kernel.instances.all.catsKernelStdOrderForDeadline

import scala.concurrent.duration.{Deadline, Duration, FiniteDuration}
import scala.collection.immutable.{BitSet, Queue, SortedMap, SortedSet}
import scala.util.Random
import java.util.UUID
import java.util.concurrent.TimeUnit.{DAYS, HOURS, MICROSECONDS, MILLISECONDS, MINUTES, NANOSECONDS, SECONDS}
import compat.scalaVersionSpecific.*
import munit.ScalaCheckSuite
import org.scalacheck.Test.Parameters

@suppressUnusedImportWarningForScalaVersionSpecific
object KernelCheck {

  implicit val arbitraryBitSet: Arbitrary[BitSet] =
    Arbitrary(arbitrary[List[Short]].map(ns => BitSet(ns.map(_ & 0xffff): _*)))

  implicit val arbitrarySymbol: Arbitrary[Symbol] =
    Arbitrary(arbitrary[String].map(s => Symbol(s)))

  implicit val arbitraryUUID: Arbitrary[UUID] =
    Arbitrary(Gen.uuid)

  implicit val arbitraryFiniteDuration: Arbitrary[FiniteDuration] = {
    // max range is +/- 292 years, but we give ourselves some extra headroom
    // to ensure that we can add these things up. they crash on overflow.
    val n = (292L * 365) / 500
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

  implicit val arbitraryDeadline: Arbitrary[Deadline] =
    Arbitrary(arbitraryFiniteDuration.arbitrary.map(Deadline.apply))

  // `Duration.Undefined`, `Duration.Inf` and `Duration.MinusInf` break the tests
  implicit val arbitraryDuration: Arbitrary[Duration] =
    Arbitrary(arbitraryFiniteDuration.arbitrary.map(fd => fd: Duration))

  // Copied from cats-laws.
  implicit def arbitrarySortedMap[K: Arbitrary: Order, V: Arbitrary]: Arbitrary[SortedMap[K, V]] =
    Arbitrary(arbitrary[Map[K, V]].map(s => SortedMap.empty[K, V](implicitly[Order[K]].toOrdering) ++ s))

  // Copied from cats-laws.
  implicit def cogenSortedMap[K: Order: Cogen, V: Cogen]: Cogen[SortedMap[K, V]] = {
    implicit val orderingK: Ordering[K] = Order[K].toOrdering

    implicitly[Cogen[Map[K, V]]].contramap(_.toMap)
  }

  // Copied from cats-laws.
  implicit def arbitrarySortedSet[A: Arbitrary: Order]: Arbitrary[SortedSet[A]] =
    Arbitrary(arbitrary[Set[A]].map(s => SortedSet.empty[A](implicitly[Order[A]].toOrdering) ++ s))

  // Copied from cats-laws.
  implicit def cogenSortedSet[A: Order: Cogen]: Cogen[SortedSet[A]] = {
    implicit val orderingA: Ordering[A] = Order[A].toOrdering

    implicitly[Cogen[Set[A]]].contramap(_.toSet)
  }

  // this instance is not available in ScalaCheck 1.13.2.
  // remove this once a newer version is available.
  implicit val cogenBigInt: Cogen[BigInt] =
    Cogen[Long].contramap(_.toLong)

  // this instance is not available in ScalaCheck 1.13.2.
  // remove this once a newer version is available.
  implicit val cogenBigDecimal: Cogen[BigDecimal] =
    Cogen[Double].contramap(_.toDouble)

  implicit val cogenSymbol: Cogen[Symbol] =
    Cogen[String].contramap(_.name)

  implicit val cogenUUID: Cogen[UUID] =
    Cogen[(Long, Long)].contramap(u => (u.getMostSignificantBits, u.getLeastSignificantBits))

  implicit val cogenDeadline: Cogen[Deadline] =
    Cogen[FiniteDuration].contramap(_.time)
}

class TestsConfig extends ScalaCheckSuite {
  // The ScalaCheck defaults (100,100) are too high for Scala.js.
  final val PropMaxSize = if (Platform.isJs) 10 else 100
  final val PropMinSuccessful = if (Platform.isJs) 10 else 100
  final val PropWorkers = if (Platform.isJvm) 2 else 1

  implicit override def scalaCheckTestParameters: Parameters =
    Parameters.default
      .withMinSuccessfulTests(PropMinSuccessful)
      .withMaxSize(PropMaxSize)
      .withWorkers(PropWorkers)
}

class Tests extends TestsConfig with DisciplineSuite {

  import KernelCheck.*

  test("The instances in scope are not ambiguous") {
    implicitly[Monoid[Option[String]]]
    implicitly[Semigroup[Option[String]]]
    implicitly[Monoid[Option[Int]]]
    implicitly[Semigroup[Option[Int]]]
    implicitly[CommutativeSemigroup[Option[Int]]]
    implicitly[CommutativeMonoid[Option[Int]]]
  }

  {
    // needed for Cogen[Map[...]]
    implicit val ohe: Ordering[HasEq[Int]] = Ordering.by[HasEq[Int], Int](_.a)
    checkAll("Eq[Map[String, HasEq[Int]]]", EqTests[Map[String, HasEq[Int]]].eqv)
    checkAll("Eq[SortedMap[String, HasEq[Int]]]", EqTests[SortedMap[String, HasEq[Int]]].eqv)
  }

  checkAll("Eq[List[HasEq[Int]]]", EqTests[List[HasEq[Int]]].eqv)
  checkAll("Eq[Option[HasEq[Int]]]", EqTests[Option[HasEq[Int]]].eqv)
  checkAll("Eq[Vector[HasEq[Int]]]", EqTests[Vector[HasEq[Int]]].eqv)
  checkAll("Eq[Stream[HasEq[Int]]]", EqTests[Stream[HasEq[Int]]].eqv)
  checkAll("Eq[Queue[HasEq[Int]]]", EqTests[Queue[HasEq[Int]]].eqv)

  checkAll("PartialOrder[Set[Int]]", PartialOrderTests[Set[Int]].partialOrder)
  checkAll("PartialOrder.reverse(PartialOrder[Set[Int]])",
           PartialOrderTests(using PartialOrder.reverse(PartialOrder[Set[Int]])).partialOrder
  )
  checkAll(
    "PartialOrder.reverse(PartialOrder.reverse(PartialOrder[Set[Int]]))",
    PartialOrderTests(using PartialOrder.reverse(PartialOrder.reverse(PartialOrder[Set[Int]]))).partialOrder
  )
  checkAll("PartialOrder[Option[HasPartialOrder[Int]]]", PartialOrderTests[Option[HasPartialOrder[Int]]].partialOrder)
  checkAll("PartialOrder[List[HasPartialOrder[Int]]]", PartialOrderTests[List[HasPartialOrder[Int]]].partialOrder)
  checkAll("PartialOrder[Vector[HasPartialOrder[Int]]]", PartialOrderTests[Vector[HasPartialOrder[Int]]].partialOrder)
  checkAll("PartialOrder[Stream[HasPartialOrder[Int]]]", PartialOrderTests[Stream[HasPartialOrder[Int]]].partialOrder)
  checkAll("PartialOrder[Queue[HasPartialOrder[Int]]]", PartialOrderTests[Queue[HasPartialOrder[Int]]].partialOrder)
  checkAll("PartialOrder[SortedMap[Int, HasPartialOrder[Int]]]",
           PartialOrderTests[SortedMap[Int, HasPartialOrder[Int]]].partialOrder
  )
  checkAll("Semilattice.asMeetPartialOrder[Set[Int]]",
           PartialOrderTests(using Semilattice.asMeetPartialOrder[Set[Int]]).partialOrder
  )
  checkAll("Semilattice.asJoinPartialOrder[Set[Int]]",
           PartialOrderTests(using Semilattice.asJoinPartialOrder[Set[Int]]).partialOrder
  )

  checkAll("Order[String]", OrderTests[String].order)
  checkAll("Order[Symbol]", OrderTests[Symbol].order)
  checkAll("Order[Byte]", OrderTests[Byte].order)
  checkAll("PartialOrder[BitSet]", PartialOrderTests[BitSet].partialOrder)
  checkAll("Order[BigInt]", OrderTests[BigInt].order)
  checkAll("Order[Duration]", OrderTests[Duration].order)
  checkAll("Order[FiniteDuration]", OrderTests[FiniteDuration].order)
  checkAll("Order[Deadline]", OrderTests[Deadline].order)
  checkAll("Order[UUID]", OrderTests[UUID].order)
  checkAll("Order[List[Int]]", OrderTests[List[Int]].order)
  checkAll("Order[Option[String]]", OrderTests[Option[String]].order)
  checkAll("Order[List[String]", OrderTests[List[String]].order)
  checkAll("Order[Vector[Int]]", OrderTests[Vector[Int]].order)
  checkAll("Order[Stream[Int]]", OrderTests[Stream[Int]].order)
  checkAll("Order[Queue[Int]]", OrderTests[Queue[Int]].order)
  checkAll("Order[SortedSet[String]", OrderTests[SortedSet[String]].order)
  checkAll("Order[SortedMap[Int, String]]", OrderTests[SortedMap[Int, String]].order)
  checkAll("fromOrdering[Int]", OrderTests(using Order.fromOrdering[Int]).order)
  checkAll("Order.reverse(Order[Int])", OrderTests(using Order.reverse(Order[Int])).order)
  checkAll("Order.reverse(Order.reverse(Order[Int]))", OrderTests(using Order.reverse(Order.reverse(Order[Int]))).order)
  checkAll("Order.fromLessThan[Int](_ < _)", OrderTests(using Order.fromLessThan[Int](_ < _)).order)

  checkAll("LowerBounded[Duration]", LowerBoundedTests[Duration].lowerBounded)
  checkAll("LowerBounded[FiniteDuration]", LowerBoundedTests[FiniteDuration].lowerBounded)
  checkAll("LowerBounded[UUID]", LowerBoundedTests[UUID].lowerBounded)
  checkAll("LowerBounded[String]", LowerBoundedTests[String].lowerBounded)
  checkAll("LowerBounded[Symbol]", LowerBoundedTests[Symbol].lowerBounded)

  checkAll("UpperBounded[Duration]", UpperBoundedTests[Duration].upperBounded)
  checkAll("UpperBounded[FiniteDuration]", UpperBoundedTests[FiniteDuration].upperBounded)
  checkAll("UpperBounded[UUID]", UpperBoundedTests[UUID].upperBounded)

  checkAll("BoundedEnumerable[Unit]", BoundedEnumerableTests[Unit].boundedEnumerable)
  checkAll("BoundedEnumerable[Boolean]", BoundedEnumerableTests[Boolean].boundedEnumerable)
  checkAll("BoundedEnumerable[Byte]", BoundedEnumerableTests[Byte].boundedEnumerable)
  checkAll("BoundedEnumerable[Short]", BoundedEnumerableTests[Short].boundedEnumerable)
  checkAll("BoundedEnumerable[Int]", BoundedEnumerableTests[Int].boundedEnumerable)
  checkAll("BoundedEnumerable[Char]", BoundedEnumerableTests[Char].boundedEnumerable)
  checkAll("BoundedEnumerable[Long]", BoundedEnumerableTests[Long].boundedEnumerable)
  checkAll(
    "BoundedEnumerable.reverse(BoundedEnumerable[Int])",
    BoundedEnumerableTests(using BoundedEnumerable.reverse(BoundedEnumerable[Int])).boundedEnumerable
  )
  checkAll(
    "BoundedEnumerable.reverse(BoundedEnumerable.reverse(BoundedEnumerable[Int]))",
    BoundedEnumerableTests(
      using BoundedEnumerable.reverse(BoundedEnumerable.reverse(BoundedEnumerable[Int]))
    ).boundedEnumerable
  )

  checkAll("Monoid[String]", MonoidTests[String].monoid)
  checkAll("Monoid[String]", SerializableTests.serializable(Monoid[String]))
  checkAll("Monoid[Option[String]]", MonoidTests[Option[String]].monoid)
  checkAll("Monoid[Option[String]]", SerializableTests.serializable(Monoid[Option[String]]))
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
  checkAll("Monoid[SortedMap[String, String]]", MonoidTests[SortedMap[String, String]].monoid)
  checkAll("Monoid[SortedMap[String, String]]", SerializableTests.serializable(Monoid[SortedMap[String, String]]))
  checkAll("Monoid[Queue[Int]]", MonoidTests[Queue[Int]].monoid)
  checkAll("Monoid[Queue[Int]]", SerializableTests.serializable(Monoid[Queue[Int]]))

  checkAll("CommutativeMonoid[Option[Int]]", CommutativeMonoidTests[Option[Int]].commutativeMonoid)
  checkAll("CommutativeMonoid[Option[Int]]", SerializableTests.serializable(CommutativeMonoid[Option[Int]]))
  checkAll("CommutativeMonoid[Map[String, Int]]", CommutativeMonoidTests[Map[String, Int]].commutativeMonoid)
  checkAll("CommutativeMonoid[Map[String, Int]]", SerializableTests.serializable(CommutativeMonoid[Map[String, Int]]))
  checkAll("CommutativeMonoid[SortedMap[String, Int]]",
           CommutativeMonoidTests[SortedMap[String, Int]].commutativeMonoid
  )
  checkAll("CommutativeMonoid[SortedMap[String, Int]]",
           SerializableTests.serializable(CommutativeMonoid[SortedMap[String, Int]])
  )

  checkAll("BoundedSemilattice[BitSet]", BoundedSemilatticeTests[BitSet].boundedSemilattice)
  checkAll("BoundedSemilattice[BitSet]", SerializableTests.serializable(BoundedSemilattice[BitSet]))
  checkAll("BoundedSemilattice[Set[Int]]", BoundedSemilatticeTests[Set[Int]].boundedSemilattice)
  checkAll("BoundedSemilattice[Set[Int]]", SerializableTests.serializable(BoundedSemilattice[Set[Int]]))
  checkAll("BoundedSemilattice[SortedSet[Int]]", BoundedSemilatticeTests[SortedSet[Int]].boundedSemilattice)
  checkAll("BoundedSemilattice[SortedSet[Int]]", SerializableTests.serializable(BoundedSemilattice[SortedSet[Int]]))

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
  checkAll("Hash[SortedMap[Int, String]]", HashTests[SortedMap[Int, String]].hash)
  checkAll("Hash[Queue[Int]", HashTests[Queue[Int]].hash)

  checkAll("Order[BigDecimal]", OrderTests[BigDecimal].order)
  checkAll("CommutativeGroup[BigDecimal]", CommutativeGroupTests[BigDecimal].commutativeGroup)
  checkAll("CommutativeGroup[BigDecimal]", SerializableTests.serializable(CommutativeGroup[BigDecimal]))

  test("CommutativeGroup[BigDecimal]'s combine should be associative for known problematic cases (#3303)") {
    import java.math.MathContext

    val one = BigDecimal("1", MathContext.DECIMAL32)
    val small = BigDecimal("0.00001111111", MathContext.DECIMAL32)
    val xs = one :: List.fill(10)(small)
    val combineRight = xs.reduceRight(CommutativeGroup[BigDecimal].combine)
    val combineLeft = xs.reduceLeft(CommutativeGroup[BigDecimal].combine)

    assert(Eq[BigDecimal].eqv(combineRight, combineLeft))
  }

  test("CommutativeGroup[BigDecimal]'s combine should be commutative for known problematic cases (#3303)") {
    import java.math.MathContext

    val one = BigDecimal("1")
    val small = BigDecimal("1e-7", MathContext.DECIMAL32)

    assert(
      Eq[BigDecimal].eqv(CommutativeGroup[BigDecimal].combine(one, small),
                         CommutativeGroup[BigDecimal].combine(small, one)
      )
    )
  }

  checkAll("Band[(Int, Int)]", BandTests[(Int, Int)].band)
  checkAll("Band[(Int, Int)]", SerializableTests.serializable(Band[(Int, Int)]))

  checkAll("BoundedSemilattice[Unit]", BoundedSemilatticeTests[Unit].boundedSemilattice)
  checkAll("BoundedSemilattice[Unit]", SerializableTests.serializable(BoundedSemilattice[Unit]))

  // Comparison related

  // Something that can give NaN for test
  def subsetPartialOrder[A]: PartialOrder[Set[A]] = (x, y) =>
    if (x == y) 0.0
    else if (x.subsetOf(y)) -1.0
    else if (y.subsetOf(x)) 1.0
    else Double.NaN

  checkAll("subsetPartialOrder[Int]", PartialOrderTests(using subsetPartialOrder[Int]).partialOrder)

  {
    implicit def subsetPartialOrdering[A]: PartialOrdering[Set[A]] =
      new PartialOrdering[Set[A]] {

        override def tryCompare(x: Set[A], y: Set[A]): Option[Int] =
          if (x == y) Some(0)
          else if (x.subsetOf(y)) Some(-1)
          else if (y.subsetOf(x)) Some(1)
          else None

        override def lteq(x: Set[A], y: Set[A]): Boolean = (x.subsetOf(y)) || (x == y)
      }
    checkAll("fromPartialOrdering[Int]",
             PartialOrderTests(using PartialOrder.fromPartialOrdering[Set[Int]]).partialOrder
    )
  }

  implicit val arbitraryComparison: Arbitrary[Comparison] =
    Arbitrary(Gen.oneOf(Comparison.GreaterThan, Comparison.EqualTo, Comparison.LessThan))

  implicit val cogenComparison: Cogen[Comparison] =
    Cogen[Int].contramap(_.toInt)

  checkAll("Eq[Comparison]", EqTests[Comparison].eqv)

  checkAll("Monoid[Comparison]", MonoidTests[Comparison].monoid)

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

  property("sign . toInt . comparison = sign . compare") {
    forAll { (i: Int, j: Int) =>
      val found = Order[Int].comparison(i, j)
      val expected = Order[Int].compare(i, j)
      Eq[Int].eqv(found.toInt.sign, expected.sign)
    }
  }

  property("sign . toDouble . partialComparison = sign . partialCompare") {
    forAll { (x: Set[Int], y: Set[Int]) =>
      val found = subsetPartialOrder[Int].partialComparison(x, y).map(_.toDouble.sign)
      val expected = Some(subsetPartialOrder[Int].partialCompare(x, y)).filter(d => !d.isNaN).map(_.sign)
      Eq[Option[Double]].eqv(found, expected)
    }
  }

  // esoteric machinery follows...

  implicit lazy val band: Band[(Int, Int)] =
    Band.instance((a, b) => (a._1, b._2))

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
    implicit val NOrderEq: Eq[Order[N]] = Eq.by { (order: Order[N]) =>
      Vector.tabulate(nMax)(N).sorted(order.toOrdering)
    }
    implicit val NEqEq: Eq[Eq[N]] = (a, b) =>
      Iterator
        .tabulate(nMax)(N)
        .flatMap(x => Iterator.tabulate(nMax)(N).map((x, _)))
        .forall { case (x, y) => a.eqv(x, y) == b.eqv(x, y) }

    implicit val monoidOrderN: Monoid[Order[N]] & Band[Order[N]] = Order.whenEqualMonoid[N]
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

final class LongRunningTests extends ScalaCheckSuite with DisciplineSuite {
  // This increases the number of successes to trigger the problem
  // described here: https://github.com/typelevel/cats/issues/3734
  // With this number of positive cases the problem is systematic
  // or at least it happens very often.
  final val PropMaxSize = if (Platform.isJs) 10 else 100
  final val PropMinSuccessful = if (Platform.isJs) 10 else 400 * 1000
  final val PropWorkers = if (Platform.isJvm) 2 else 1

  implicit override def scalaCheckTestParameters: Parameters =
    Parameters.default
      .withMinSuccessfulTests(PropMinSuccessful)
      .withMaxSize(PropMaxSize)
      .withWorkers(PropWorkers)

  import KernelCheck.*

  checkAll("Deeper test of Eq[Duration]", EqTests[Duration].eqv)
  checkAll("Deeper test of Eq[FiniteDuration]", EqTests[FiniteDuration].eqv)
}
