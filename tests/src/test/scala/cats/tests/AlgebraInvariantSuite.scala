package cats.tests

import cats.{CommutativeApplicative, CommutativeApply, Invariant, InvariantMonoidal}
import cats.kernel._
import cats.kernel.laws.discipline.{GroupTests, MonoidTests, SemigroupTests, _}
import cats.laws.discipline.{
  ExhaustiveCheck,
  InvariantMonoidalTests,
  InvariantSemigroupalTests,
  InvariantTests,
  MiniFloat,
  MiniInt,
  SerializableTests
}
import cats.laws.discipline.MiniInt._
import cats.laws.discipline.eq._
import cats.laws.discipline.arbitrary._
import cats.syntax.invariant._
import cats.syntax.order._
import org.scalacheck.{Arbitrary, Gen}

class AlgebraInvariantSuite extends CatsSuite with ScalaVersionSpecificAlgebraInvariantSuite {
  // working around https://github.com/typelevel/cats/issues/2701
  implicit private val eqSetBooleanTuple: Eq[(Set[Boolean], Set[Boolean])] = Eq.fromUniversalEquals
  implicit private val eqSetBooleanBooleanTuple: Eq[(Set[Boolean], Boolean)] = Eq.fromUniversalEquals

  catsLawsEqForBand[Set[Boolean]]

  // https://github.com/typelevel/cats/issues/2725
  implicit private def commutativeMonoidForSemigroup[A](implicit
    csA: CommutativeSemigroup[A]
  ): CommutativeMonoid[Option[A]] =
    new CommutativeMonoid[Option[A]] {
      def empty: Option[A] = None
      def combine(x: Option[A], y: Option[A]): Option[A] =
        (x, y) match {
          case (None, r)          => r
          case (l, None)          => l
          case (Some(l), Some(r)) => Some(csA.combine(l, r))
        }
    }

  private def leftOptionMonoid[A]: Monoid[Option[A]] =
    new Monoid[Option[A]] {
      def empty: Option[A] = None
      def combine(x: Option[A], y: Option[A]): Option[A] = x
    }

  private def rightOptionMonoid[A]: Monoid[Option[A]] =
    new Monoid[Option[A]] {
      def empty: Option[A] = None
      def combine(x: Option[A], y: Option[A]): Option[A] = y
    }

  private val boundedSemilatticeMiniInt: BoundedSemilattice[MiniInt] = new BoundedSemilattice[MiniInt] {
    def empty: MiniInt = MiniInt.zero
    def combine(x: MiniInt, y: MiniInt): MiniInt = x | y
  }

  private val genBoundedSemilatticeMiniInt: Gen[BoundedSemilattice[MiniInt]] =
    Gen.const(miniIntOr)

  private val genCommutativeGroupInt: Gen[CommutativeGroup[Int]] =
    Gen.const(implicitly[CommutativeGroup[Int]])

  private val miniIntMultiplication: CommutativeMonoid[MiniInt] = new CommutativeMonoid[MiniInt] {
    val empty = MiniInt.one
    def combine(x: MiniInt, y: MiniInt): MiniInt = x * y
  }

  private val maxMiniInt: CommutativeMonoid[MiniInt] = new CommutativeMonoid[MiniInt] {
    val empty = MiniInt.minValue
    def combine(x: MiniInt, y: MiniInt): MiniInt = if (x > y) x else y
  }

  private val genMonoidMiniInt: Gen[Monoid[MiniInt]] =
    Gen.oneOf(miniIntAddition, miniIntMultiplication, maxMiniInt)

  private val genMonoidOptionMiniInt: Gen[Monoid[Option[MiniInt]]] =
    Gen.oneOf(
      commutativeMonoidForSemigroup(miniIntAddition),
      commutativeMonoidForSemigroup(miniIntMultiplication),
      commutativeMonoidForSemigroup(maxMiniInt),
      leftOptionMonoid[MiniInt],
      rightOptionMonoid[MiniInt]
    )

  private val genCommutativeMonoidMiniInt: Gen[CommutativeMonoid[MiniInt]] =
    Gen.oneOf(miniIntAddition, miniIntMultiplication, maxMiniInt)

  private val genCommutativeGroupMiniInt: Gen[CommutativeGroup[MiniInt]] =
    Gen.const(miniIntAddition)

  implicit private val arbMonoidOptionMiniInt: Arbitrary[Monoid[Option[MiniInt]]] =
    Arbitrary(genMonoidOptionMiniInt)

  implicit private val arbSemigroupOptionMiniInt: Arbitrary[Semigroup[Option[MiniInt]]] =
    Arbitrary(genMonoidOptionMiniInt)

  implicit private val arbSemigroupMiniInt: Arbitrary[Semigroup[MiniInt]] =
    Arbitrary(genMonoidMiniInt)

  implicit private val arbCommutativeMonoidMiniInt: Arbitrary[CommutativeMonoid[MiniInt]] =
    Arbitrary(genCommutativeMonoidMiniInt)

  implicit private val arbCommutativeSemigroupMiniInt: Arbitrary[CommutativeSemigroup[MiniInt]] =
    Arbitrary(genCommutativeMonoidMiniInt)

  implicit private val arbGroupMiniInt: Arbitrary[Group[MiniInt]] =
    Arbitrary(genCommutativeGroupMiniInt)

  implicit private val arbCommutativeGroupMiniInt: Arbitrary[CommutativeGroup[MiniInt]] =
    Arbitrary(genCommutativeGroupMiniInt)

  private val boolAnd: CommutativeMonoid[Boolean] = new CommutativeMonoid[Boolean] {
    val empty = true
    def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  private val boolOr: CommutativeMonoid[Boolean] = new CommutativeMonoid[Boolean] {
    val empty = false
    def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  private val genMonoidOptionBoolean: Gen[Monoid[Option[Boolean]]] =
    Gen.oneOf(commutativeMonoidForSemigroup(boolAnd),
              commutativeMonoidForSemigroup(boolOr),
              leftOptionMonoid[Boolean],
              rightOptionMonoid[Boolean]
    )

  implicit private val arbMonoidOptionBoolean: Arbitrary[Monoid[Option[Boolean]]] =
    Arbitrary(genMonoidOptionBoolean)

  implicit private val arbSemibroupOptionBoolean: Arbitrary[Semigroup[Option[Boolean]]] =
    Arbitrary(genMonoidOptionBoolean)

  private val genCommutativeMonoidBoolean: Gen[CommutativeMonoid[Boolean]] =
    Gen.oneOf(boolAnd, boolOr)

  implicit private val arbCommutativeMonoidBoolean: Arbitrary[CommutativeMonoid[Boolean]] =
    Arbitrary(genCommutativeMonoidBoolean)

  implicit private val arbCommutativeSemigroupBoolean: Arbitrary[CommutativeSemigroup[Boolean]] =
    Arbitrary(genCommutativeMonoidBoolean)

  implicit private val arbBandSetMiniInt: Arbitrary[Band[MiniInt]] =
    Arbitrary(genBoundedSemilatticeMiniInt)

  implicit private val arbSemilatticeSetMiniInt: Arbitrary[Semilattice[MiniInt]] =
    Arbitrary(genBoundedSemilatticeMiniInt)

  implicit private val arbBoundedSemilatticeSetMiniInt: Arbitrary[BoundedSemilattice[MiniInt]] =
    Arbitrary(genBoundedSemilatticeMiniInt)

  implicit private val arbGroupInt: Arbitrary[Group[Int]] =
    Arbitrary(genCommutativeGroupInt)

  implicit private val arbCommutativeGroupInt: Arbitrary[CommutativeGroup[Int]] =
    Arbitrary(genCommutativeGroupInt)

  implicit private val arbNumericMiniInt: Arbitrary[Numeric[MiniInt]] =
    Arbitrary(Gen.const(integralForMiniInt))

  implicit private val arbIntegralMiniInt: Arbitrary[Integral[MiniInt]] =
    Arbitrary(Gen.const(integralForMiniInt))

  implicit private val arbFractionalMiniFloat: Arbitrary[Fractional[MiniFloat]] =
    Arbitrary(Gen.const(fractionalForMiniFloat))

  implicit protected def eqNumeric[A: Eq: ExhaustiveCheck]: Eq[Numeric[A]] = {
    // In order to test .toFloat and .toDouble we introduce local Eq instances that compare NaN instances as equivalent
    implicit val nanInclusiveFloatEq: Eq[Float] = Eq.or(Eq.by(_.isNaN), Eq.catsKernelInstancesForFloat)
    implicit val nanInclusiveDoubleEq: Eq[Double] = Eq.or(Eq.by(_.isNaN), Eq.catsKernelInstancesForDouble)

    val versionAgnosticNumericEq: Eq[Numeric[A]] = Eq.by { (numeric: Numeric[A]) =>
      // This allows us to catch the case where the fromInt overflows. We use the None to compare two Numeric instances,
      // verifying that when fromInt throws for one, it throws for the other.
      val fromMiniInt: MiniInt => Option[A] =
        miniInt =>
          try Some(numeric.fromInt(miniInt.toInt))
          catch {
            case _: IllegalArgumentException => None // MiniInt overflow
          }

      (
        numeric.compare _,
        numeric.plus _,
        numeric.minus _,
        numeric.times _,
        numeric.negate _,
        fromMiniInt,
        numeric.toInt _,
        numeric.toLong _,
        numeric.toFloat _,
        numeric.toDouble _
      )
    }

    Eq.and(versionSpecificNumericEq, versionAgnosticNumericEq)
  }

  implicit protected def eqIntegral[A: Eq: ExhaustiveCheck]: Eq[Integral[A]] = {
    def makeDivisionOpSafe(unsafeF: (A, A) => A): (A, A) => Option[A] =
      (x, y) =>
        try Some(unsafeF(x, y))
        catch {
          case _: ArithmeticException      => None // division by zero
          case _: IllegalArgumentException => None // overflow
        }

    Eq.by { (integral: Integral[A]) =>
      // Integral.quot and Integral.rem throw on division by zero, and the underlying integral type (eg MiniInt) can
      // overflow. We catch these cases here and lift them into Option so as to test that two Integrals are equal only
      // when they both throw for the same input.
      val safeQuot: (A, A) => Option[A] = makeDivisionOpSafe(integral.quot)
      val safeRem: (A, A) => Option[A] = makeDivisionOpSafe(integral.rem)

      (
        integral: Numeric[A],
        safeQuot,
        safeRem
      )
    }
  }

  implicit protected def eqFractional[A: Eq: ExhaustiveCheck]: Eq[Fractional[A]] = {
    Eq.by { (fractional: Fractional[A]) =>
      (
        fractional: Numeric[A],
        fractional.div _
      )
    }
  }

  checkAll("InvariantMonoidal[Semigroup]", SemigroupTests[Int](InvariantMonoidal[Semigroup].point(0)).semigroup)
  checkAll("InvariantMonoidal[CommutativeSemigroup]",
           CommutativeSemigroupTests[Int](InvariantMonoidal[CommutativeSemigroup].point(0)).commutativeSemigroup
  )

  checkAll("InvariantSemigroupal[Monoid]",
           InvariantSemigroupalTests[Monoid].invariantSemigroupal[Option[MiniInt], Option[Boolean], Option[Boolean]]
  )

  checkAll("Invariant[Numeric]", InvariantTests[Numeric].invariant[MiniInt, Boolean, Boolean])
  checkAll("Invariant[Integral]", InvariantTests[Integral].invariant[MiniInt, Boolean, Boolean])
  checkAll("Invariant[Fractional]", InvariantTests[Fractional].invariant[MiniFloat, Boolean, Boolean])

  {
    val S: Semigroup[Int] = Semigroup[Int].imap(identity)(identity)
    checkAll("Semigroup[Int]", SemigroupTests[Int](S).semigroup)
  }

  {
    val S: Monoid[Int] = Monoid[Int].imap(identity)(identity)
    checkAll("Monoid[Int]", MonoidTests[Int](S).monoid)
  }

  {
    val S: Group[Int] = Group[Int].imap(identity)(identity)
    checkAll("Group[Int]", GroupTests[Int](S).group)
  }

  {
    val S: CommutativeSemigroup[Int] = CommutativeSemigroup[Int].imap(identity)(identity)
    checkAll("CommutativeSemigroup[Int]", CommutativeSemigroupTests[Int](S).commutativeSemigroup)
  }

  {
    val S: CommutativeSemigroup[Option[Int]] = CommutativeApply.commutativeSemigroupFor[Option, Int]
    checkAll("CommutativeSemigroup[Option[Int]", CommutativeSemigroupTests[Option[Int]](S).commutativeSemigroup)
  }

  {
    val S: CommutativeMonoid[Option[Int]] = CommutativeApplicative.commutativeMonoidFor[Option, Int]
    checkAll("CommutativeMonoid[Option[Int]", CommutativeMonoidTests[Option[Int]](S).commutativeMonoid)
  }

  {
    val S: CommutativeMonoid[Int] = CommutativeMonoid[Int].imap(identity)(identity)
    checkAll("CommutativeMonoid[Int]", CommutativeMonoidTests[Int](S).commutativeMonoid)
  }

  {
    checkAll("CommutativeMonoid[MiniInt]", CommutativeMonoidTests[MiniInt](miniIntAddition).commutativeMonoid)
  }

  {
    val S: CommutativeGroup[Int] = CommutativeGroup[Int].imap(identity)(identity)
    checkAll("CommutativeGroup[Int]", CommutativeGroupTests[Int](S).commutativeGroup)
  }

  {
    val S: Band[Set[Int]] = Band[Set[Int]].imap(identity)(identity)
    checkAll("Band[Set[Int]]", BandTests[Set[Int]](S).band)
  }

  {
    val S: Semilattice[Set[Int]] = Semilattice[Set[Int]].imap(identity)(identity)
    checkAll("Semilattice[Set[Int]]", SemilatticeTests[Set[Int]](S).semilattice)
  }

  {
    val S: BoundedSemilattice[Set[Int]] = BoundedSemilattice[Set[Int]].imap(identity)(identity)
    checkAll("BoundedSemilattice[Set[Int]]", BoundedSemilatticeTests[Set[Int]](S).boundedSemilattice)
  }

  checkAll("Invariant[Semigroup]", InvariantTests[Semigroup].invariant[MiniInt, Boolean, Boolean])
  checkAll("Invariant[Semigroup]", SerializableTests.serializable(Invariant[Semigroup]))

  checkAll("Invariant[CommutativeSemigroup]", InvariantTests[CommutativeSemigroup].invariant[MiniInt, Boolean, Boolean])
  checkAll("Invariant[CommutativeSemigroup]", SerializableTests.serializable(Invariant[CommutativeSemigroup]))

  checkAll("Invariant[Band]", InvariantTests[Band].invariant[MiniInt, Set[Boolean], Set[Boolean]])
  checkAll("Invariant[Band]", SerializableTests.serializable(Invariant[Band]))

  checkAll("Invariant[Monoid]", InvariantTests[Monoid].invariant[Option[MiniInt], Boolean, Boolean])
  checkAll("Invariant[Monoid]", SerializableTests.serializable(Invariant[Monoid]))

  checkAll("Invariant[Semilattice]", InvariantTests[Semilattice].invariant[MiniInt, Set[Boolean], Set[Boolean]])
  checkAll("Invariant[Semilattice]", SerializableTests.serializable(Invariant[Semilattice]))

  checkAll("Invariant[CommutativeMonoid]", InvariantTests[CommutativeMonoid].invariant[MiniInt, Boolean, Boolean])
  checkAll("Invariant[CommutativeMonoid]", SerializableTests.serializable(Invariant[CommutativeMonoid]))

  checkAll("Invariant[BoundedSemilattice]",
           InvariantTests[BoundedSemilattice].invariant[MiniInt, Set[Boolean], Set[Boolean]]
  )
  checkAll("Invariant[BoundedSemilattice]", SerializableTests.serializable(Invariant[BoundedSemilattice]))

  checkAll("Invariant[Group]", InvariantTests[Group].invariant[MiniInt, Boolean, Boolean])
  checkAll("Invariant[Group]", SerializableTests.serializable(Invariant[Group]))

  checkAll("Invariant[CommutativeGroup]", InvariantTests[CommutativeGroup].invariant[MiniInt, Boolean, Boolean])
  checkAll("Invariant[CommutativeGroup]", SerializableTests.serializable(Invariant[CommutativeGroup]))

  checkAll("InvariantMonoidal[Semigroup]",
           InvariantMonoidalTests[Semigroup].invariantMonoidal[Option[MiniInt], Option[Boolean], Option[Boolean]]
  )
  checkAll("InvariantMonoidal[Semigroup]", SerializableTests.serializable(InvariantMonoidal[Semigroup]))

  checkAll("InvariantMonoidal[CommutativeSemigroup]",
           InvariantMonoidalTests[CommutativeSemigroup].invariantMonoidal[MiniInt, Boolean, Boolean]
  )
  checkAll("InvariantMonoidal[CommutativeSemigroup]",
           SerializableTests.serializable(InvariantMonoidal[CommutativeSemigroup])
  )

}
