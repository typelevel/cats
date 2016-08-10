package cats.kernel
package laws

import catalysts.Platform
import catalysts.macros.TypeTagM

import cats.kernel.instances.all._

import org.typelevel.discipline.{ Laws }
import org.typelevel.discipline.scalatest.Discipline
import org.scalacheck.{ Arbitrary, Gen }
import Arbitrary.arbitrary
import org.scalatest.FunSuite
import scala.util.Random

class LawTests extends FunSuite with Discipline {

  // The scalacheck defaults (100,100) are too high for scala-js.
  final val PropMaxSize = if (Platform.isJs) 10 else 100
  final val PropMinSuccessful = if (Platform.isJs) 10 else 100

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(maxSize = PropMaxSize, minSuccessful = PropMinSuccessful)

  implicit def orderLaws[A: Eq: Arbitrary] = OrderLaws[A]
  implicit def groupLaws[A: Eq: Arbitrary] = GroupLaws[A]

  laws[OrderLaws, Map[String, HasEq[Int]]].check(_.eqv)
  laws[OrderLaws, List[HasEq[Int]]].check(_.eqv)
  laws[OrderLaws, Option[HasEq[Int]]].check(_.eqv)
  laws[OrderLaws, Vector[HasEq[Int]]].check(_.eqv)
  laws[OrderLaws, Stream[HasEq[Int]]].check(_.eqv)

  laws[OrderLaws, Set[Int]].check(_.partialOrder)
  laws[OrderLaws, Set[Int]]("reverse").check(_.partialOrder(PartialOrder[Set[Int]].reverse))
  laws[OrderLaws, Set[Int]]("reverse.reverse").check(_.partialOrder(PartialOrder[Set[Int]].reverse.reverse))
  laws[OrderLaws, Option[HasPartialOrder[Int]]].check(_.partialOrder)
  laws[OrderLaws, List[HasPartialOrder[Int]]].check(_.partialOrder)
  laws[OrderLaws, Vector[HasPartialOrder[Int]]].check(_.partialOrder)
  laws[OrderLaws, Stream[HasPartialOrder[Int]]].check(_.partialOrder)
  laws[OrderLaws, Set[Int]]("asMeetPartialOrder").check(_.partialOrder(Semilattice.asMeetPartialOrder[Set[Int]]))
  laws[OrderLaws, Set[Int]]("asJoinPartialOrder").check(_.partialOrder(Semilattice.asJoinPartialOrder[Set[Int]]))

  laws[OrderLaws, Unit].check(_.order)
  laws[OrderLaws, Boolean].check(_.order)
  laws[OrderLaws, String].check(_.order)
  laws[OrderLaws, Byte].check(_.order)
  laws[OrderLaws, Short].check(_.order)
  laws[OrderLaws, Char].check(_.order)
  laws[OrderLaws, Int].check(_.order)
  laws[OrderLaws, Long].check(_.order)
  laws[OrderLaws, BigInt].check(_.order)
  laws[OrderLaws, List[Int]].check(_.order)
  laws[OrderLaws, Option[String]].check(_.order)
  laws[OrderLaws, List[String]].check(_.order)
  laws[OrderLaws, Vector[Int]].check(_.order)
  laws[OrderLaws, Stream[Int]].check(_.order)
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

  laws[GroupLaws, Unit].check(_.commutativeGroup)
  laws[GroupLaws, Byte].check(_.commutativeGroup)
  laws[GroupLaws, Short].check(_.commutativeGroup)
  laws[GroupLaws, Int].check(_.commutativeGroup)
  laws[GroupLaws, Long].check(_.commutativeGroup)
  //laws[GroupLaws, Float].check(_.commutativeGroup) // approximately associative
  //laws[GroupLaws, Double].check(_.commutativeGroup) // approximately associative
  laws[GroupLaws, BigInt].check(_.commutativeGroup)

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
  }

  case class HasPartialOrder[A](a: A)

  object HasPartialOrder {
    implicit def hasPartialOrder[A: PartialOrder]: PartialOrder[HasPartialOrder[A]] =
      PartialOrder[A].on(_.a)
    implicit def hasPartialOrderArbitrary[A: Arbitrary]: Arbitrary[HasPartialOrder[A]] =
      Arbitrary(arbitrary[A].map(HasPartialOrder(_)))
  }

  case class LawChecker[L <: Laws](name: String, laws: L) {
    def check(f: L => L#RuleSet): Unit = checkAll(name, f(laws))
  }

  private[laws] def laws[L[_] <: Laws, A](implicit lws: L[A], tag: TypeTagM[A]): LawChecker[L[A]] =
    laws[L, A]("")

  private[laws] def laws[L[_] <: Laws, A](extraTag: String)(implicit laws: L[A], tag: TypeTagM[A]): LawChecker[L[A]] =
    LawChecker("[" + tag.name.toString + (if(extraTag != "") "@@" + extraTag else "") + "]", laws)

}
