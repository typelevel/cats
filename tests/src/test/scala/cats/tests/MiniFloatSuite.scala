package cats.tests

import cats.kernel.laws.discipline.{HashTests, OrderTests, SerializableTests}
import cats.kernel.{Hash, Order}
import cats.laws.discipline.MiniFloat
import cats.laws.discipline.MiniFloat._
import cats.laws.discipline.arbitrary._
import cats.syntax.eq._
import cats.tests.MiniFloatSuite.FloatArithmeticOp
import munit.Location
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen}

class MiniFloatSuite extends CatsSuite {

  // checks on allValues

  test("allValues contains no duplicates") {
    val occurrencesPerFloatValue = MiniFloat.allValues
      .groupBy(_.toFloat)
      .view

    val duplicates: List[(Float, List[MiniFloat])] = occurrencesPerFloatValue.toList.filter(_._2.size > 1)

    assert(duplicates.isEmpty, s"Minifloats with duplicate values $duplicates")
  }

  private def testAllValuesContains(value: MiniFloat)(implicit loc: Location): Unit =
    test(s"allValues contains $value") {
      MiniFloat.allValues.contains(value)
    }

  testAllValuesContains(MiniFloat.Zero)
  testAllValuesContains(MiniFloat.NegativeOne)
  testAllValuesContains(MiniFloat.One)
  testAllValuesContains(MiniFloat.MaxValue)
  testAllValuesContains(MiniFloat.MinValue)
  testAllValuesContains(MiniFloat.MinPositiveValue)
  testAllValuesContains(MiniFloat.PositiveInfinity)
  testAllValuesContains(MiniFloat.NegativeInfinity)
  testAllValuesContains(MiniFloat.NaN)

  test("allValues has max") {
    val maxInAllValues = MiniFloat.allValues.filter(_.isFinite).maxBy(_.toFloat)

    assert(maxInAllValues === MiniFloat.MaxValue)
  }

  test("allValues has min") {
    val minInAllValues = MiniFloat.allValues.filter(_.isFinite).minBy(_.toFloat)

    assert(minInAllValues === MiniFloat.MinValue)
  }

  test("behaves the same as Float for all operations on some key values") {
    val genKeyMiniFloatValues: Gen[MiniFloat] = Gen.oneOf(
      MiniFloat.NegativeInfinity,
      MiniFloat.NegativeOne,
      MiniFloat.Zero,
      MiniFloat.One,
      MiniFloat.PositiveInfinity,
      MiniFloat.NaN
    )

    forAll(FloatArithmeticOp.arb.arbitrary, genKeyMiniFloatValues, genKeyMiniFloatValues) {
      (op: FloatArithmeticOp, left: MiniFloat, right: MiniFloat) =>
        val testHint = s"$left ${op.char} $right"
        val expectedAsFloat: Float = op.forFloat(left.toFloat, right.toFloat)

        // This is necessary since Float.NaN != Float.NaN
        if (expectedAsFloat.isNaN) {
          assert(op.forMiniFloat(left, right).isNaN, testHint)
        } else {
          assert(op.forMiniFloat(left, right).toFloat === expectedAsFloat, testHint)
        }
    }
  }

  // Float conversions

  private def testFloatConversion(float: Float, expected: MiniFloat)(implicit loc: Location): Unit =
    test(s"MiniFloat.fromFloat($float) === $expected") {
      assert(MiniFloat.from(float) === expected)
    }

  private def testFloatConversion(float: Float, expected: Float)(implicit loc: Location): Unit =
    test(s"MiniFloat.fromFloat($float) === $expected") {
      val miniFloat = MiniFloat.from(float)

      // This is necessary since Float.NaN != Float.NaN
      if (miniFloat.isNaN) {
        assert(expected.isNaN)
      } else {
        assert(miniFloat.toFloat === expected)
      }
    }

  testFloatConversion(Float.NegativeInfinity, MiniFloat.NegativeInfinity)
  testFloatConversion(Float.MinValue, MiniFloat.NegativeInfinity)
  testFloatConversion(-16f, MiniFloat.NegativeInfinity)
  testFloatConversion(Math.nextDown(-12f), MiniFloat.NegativeInfinity)
  testFloatConversion(-12f, -8f)
  testFloatConversion(Math.nextDown(-6f), -8f)
  testFloatConversion(-6f, -4f)
  testFloatConversion(Math.nextDown(-3f), -4f)
  testFloatConversion(-3f, -2f)
  testFloatConversion(-1f, MiniFloat.NegativeOne)
  testFloatConversion(1f, MiniFloat.One)
  testFloatConversion(0f, MiniFloat.Zero)
  testFloatConversion(Float.MinPositiveValue, MiniFloat.Zero)
  testFloatConversion(-0f, MiniFloat.Zero)
  testFloatConversion(Math.nextDown(0.125f), MiniFloat.Zero)
  testFloatConversion(0.125f, MiniFloat.Zero)
  testFloatConversion(Math.nextDown(0.25f), MiniFloat.Zero)
  testFloatConversion(0.25f, MiniFloat.Zero)
  testFloatConversion(0.5f, 0.5f)
  testFloatConversion(1f, 1f)
  testFloatConversion(2f, 2f)
  testFloatConversion(Math.nextDown(3f), 2f)
  testFloatConversion(3f, 4f)
  testFloatConversion(4f, 4f)
  testFloatConversion(5f, 4f)
  testFloatConversion(6f, 8f)
  testFloatConversion(7f, 8f)
  testFloatConversion(8f, 8f)
  testFloatConversion(Math.nextDown(12f), 8f)
  testFloatConversion(12f, MiniFloat.PositiveInfinity)
  testFloatConversion(16f, MiniFloat.PositiveInfinity)
  testFloatConversion(Float.MaxValue, MiniFloat.PositiveInfinity)
  testFloatConversion(Float.PositiveInfinity, MiniFloat.PositiveInfinity)
  testFloatConversion(Float.NaN, MiniFloat.NaN)

  test("MiniFloat.fromFloat(-0) is not negative") {
    assert(!(MiniFloat.from(-0).toFloat < 0))
  }

  test(s"fromDouble consistent with fromFloat") {
    forAll { (n: Double) =>
      assert(MiniFloat.from(n) === MiniFloat.from(n.toFloat), n)
    }
  }

  test(s"toDouble consistent with toFloat") {
    forAll { (mf: MiniFloat) =>
      val fromToDouble = mf.toDouble.toFloat
      val fromToFloat = mf.toFloat

      // This is necessary since Float.NaN != Float.NaN
      if (fromToDouble.isNaN) {
        assert(fromToFloat.isNaN)
      } else {
        assert(fromToDouble === fromToFloat, mf)
      }
    }
  }

  // Special number tests and behaviours

  private def testSpecialNumberExpectations(
    mf: MiniFloat,
    expectIsNaN: Boolean,
    expectIsFinite: Boolean
  )(implicit
    loc: Location
  ): Unit = {
    test(s"$mf isNan $expectIsNaN") {
      assert(mf.isNaN === expectIsNaN)
    }

    test(s"$mf isFinite $expectIsFinite") {
      assert(mf.isFinite === expectIsFinite)
    }
  }

  testSpecialNumberExpectations(MiniFloat.NaN, expectIsNaN = true, expectIsFinite = false)
  testSpecialNumberExpectations(MiniFloat.PositiveInfinity, expectIsNaN = false, expectIsFinite = false)
  testSpecialNumberExpectations(MiniFloat.NegativeInfinity, expectIsNaN = false, expectIsFinite = false)
  testSpecialNumberExpectations(MiniFloat.Zero, expectIsNaN = false, expectIsFinite = true)
  testSpecialNumberExpectations(MiniFloat.MaxValue, expectIsNaN = false, expectIsFinite = true)
  testSpecialNumberExpectations(MiniFloat.MinValue, expectIsNaN = false, expectIsFinite = true)
  testSpecialNumberExpectations(MiniFloat.MinPositiveValue, expectIsNaN = false, expectIsFinite = true)

  // Negation

  test("negate zero is zero") {
    assert(-MiniFloat.Zero === MiniFloat.Zero)
  }

  test("negate one is one") {
    assert(-MiniFloat.One === MiniFloat.NegativeOne)
  }

  test("negate ∞ is -∞") {
    assert(-MiniFloat.PositiveInfinity === MiniFloat.NegativeInfinity)
  }

  test("negate -∞ is ∞") {
    assert(-MiniFloat.NegativeInfinity === MiniFloat.PositiveInfinity)
  }

  test("negate NaN is NaN") {
    assert((-MiniFloat.NaN).isNaN)
  }

  test("negation inverse") {
    forAll { (mf: MiniFloat) =>
      assert((-(-mf) === mf) || mf.isNaN)
    }
  }

  // Addition

  test("add commutative") {
    forAll { (left: MiniFloat, right: MiniFloat) =>
      assert(left + right === right + left)
    }
  }

  test("zero addition identity") {
    forAll { (mf: MiniFloat) =>
      assert(mf + MiniFloat.Zero === mf)
    }
  }

  test("max plus 1") {
    assert(MiniFloat.MaxValue + MiniFloat.One === MiniFloat.MaxValue)
  }

  test("max plus max") {
    assert(MiniFloat.MaxValue + MiniFloat.MaxValue === MiniFloat.PositiveInfinity)
  }

  test("∞ + ∞") {
    assert(MiniFloat.PositiveInfinity + MiniFloat.PositiveInfinity === MiniFloat.PositiveInfinity)
  }

  test("∞ + 1") {
    assert(MiniFloat.PositiveInfinity + MiniFloat.One === MiniFloat.PositiveInfinity)
  }

  test("∞ + (-∞)") {
    assert((MiniFloat.PositiveInfinity + MiniFloat.NegativeInfinity).isNaN)
  }

  test("NaN addition") {
    forAll { (mf: MiniFloat) =>
      assert((mf + MiniFloat.NaN).isNaN)
    }
  }

  // Subtraction

  test("subtract consistent with addition") {
    forAll { (left: MiniFloat, right: MiniFloat) =>
      assert((left + (-right)) === left - right)
    }
  }

  test("NaN subtraction") {
    forAll { (mf: MiniFloat) =>
      assert((mf - MiniFloat.NaN).isNaN)
    }
  }

  test("∞ - ∞") {
    assert((MiniFloat.PositiveInfinity - MiniFloat.PositiveInfinity).isNaN)
  }

  // Multiplication

  test("multiplication commutative") {
    forAll { (left: MiniFloat, right: MiniFloat) =>
      assert(left * right === right * left)
    }
  }

  test("one multiplicative identity") {
    forAll { (mf: MiniFloat) =>
      assert(mf * MiniFloat.One === mf)
    }
  }

  test("max * 1") {
    assert(MiniFloat.MaxValue * MiniFloat.One === MiniFloat.MaxValue)
  }

  test("max * max") {
    assert(MiniFloat.MaxValue * MiniFloat.MaxValue === MiniFloat.PositiveInfinity)
  }

  test("∞ * ∞") {
    assert(MiniFloat.PositiveInfinity * MiniFloat.PositiveInfinity === MiniFloat.PositiveInfinity)
  }

  test("∞ * 1") {
    assert(MiniFloat.PositiveInfinity * MiniFloat.One === MiniFloat.PositiveInfinity)
  }

  test("∞ * (-∞)") {
    assert(MiniFloat.PositiveInfinity * MiniFloat.NegativeInfinity === MiniFloat.NegativeInfinity)
  }

  test("NaN multiplication") {
    forAll { (mf: MiniFloat) =>
      assert((mf * MiniFloat.NaN).isNaN)
    }
  }

  // Division

  test("divide by zero") {
    forAll { (mf: MiniFloat) =>
      val result = mf / MiniFloat.Zero

      if (mf.isNaN || mf === MiniFloat.Zero) {
        assert(result.isNaN)
      } else if (mf.toFloat < 0f) {
        assert(result === MiniFloat.NegativeInfinity)
      } else {
        assert(result === MiniFloat.PositiveInfinity)
      }
    }
  }

  test("division consistent with float division") {
    forAll { (left: MiniFloat, right: MiniFloat) =>
      val result = left / right

      if (result.isNaN) {
        assert(MiniFloat.from(left.toFloat / right.toFloat).isNaN)
      } else {
        assert(result === MiniFloat.from(left.toFloat / right.toFloat))
      }
    }
  }

  // Order

  test("ordering consistent with float") {
    forAll { (left: MiniFloat, right: MiniFloat) =>
      assert(
        implicitly[Order[MiniFloat]].compare(left, right) ===
          implicitly[Order[Float]].compare(left.toFloat, right.toFloat)
      )
    }
  }

  test("float roundtrip") {
    forAll { (f: MiniFloat) =>
      assert(MiniFloat.from(f.toFloat) === f)
    }
  }

  checkAll("MiniFloat", OrderTests[MiniFloat].order)
  checkAll("Order[MiniFloat]", SerializableTests.serializable(Order[MiniFloat]))

  checkAll("MiniFloat", HashTests[MiniFloat].hash)
  checkAll("Hash[MiniFloat]", SerializableTests.serializable(Hash[MiniFloat]))

}

object MiniFloatSuite {

  sealed abstract private class FloatArithmeticOp(
    val char: Char,
    val forMiniFloat: (MiniFloat, MiniFloat) => MiniFloat,
    val forFloat: (Float, Float) => Float
  )

  private object FloatArithmeticOp {
    case object Addition extends FloatArithmeticOp('+', _ + _, _ + _)
    case object Multiplication extends FloatArithmeticOp('*', _ * _, _ * _)
    case object Subtraction extends FloatArithmeticOp('-', _ - _, _ - _)
    case object Division extends FloatArithmeticOp('/', _ / _, _ / _)

    implicit val arb: Arbitrary[FloatArithmeticOp] =
      Arbitrary(Gen.oneOf(Addition, Multiplication, Subtraction, Division))
  }

}
