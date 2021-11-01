package cats
package laws
package discipline

import cats.implicits.{catsSyntaxPartialOrder, toTraverseFilterOps}
import cats.kernel.{BoundedSemilattice, CommutativeMonoid}

/**
 * Similar to `Float`, but with a much smaller domain. The exact range of [[MiniFloat]] may be tuned from time to time,
 * so consumers of this type should avoid depending on its exact range.
 *
 * `MiniFloat` has overflow and floating-point error characteristics similar to `Float`, but these are exaggerated due
 * to its small domain. It is only approximately commutative and associative under addition and multiplication, due to
 * floating-point errors, overflows, and the behaviour of `NaN`.
 *
 * Note that unlike `Float`, `MiniFloat` does not support the representation of negative zero (`-0f`).
 */
sealed abstract class MiniFloat private (val toFloat: Float) {
  def toDouble: Double = toFloat.toDouble
  def toInt: Int = toFloat.toInt
  def toLong: Long = toFloat.toLong

  def +(that: MiniFloat): MiniFloat = MiniFloat.from(this.toFloat + that.toFloat)
  def -(that: MiniFloat): MiniFloat = MiniFloat.from(this.toFloat - that.toFloat)
  def *(that: MiniFloat): MiniFloat = MiniFloat.from(this.toFloat * that.toFloat)
  def /(that: MiniFloat): MiniFloat = MiniFloat.from(this.toFloat / that.toFloat)
  def unary_- : MiniFloat = MiniFloat.from(-this.toFloat)

  def isNaN: Boolean = toFloat.isNaN
  def isFinite: Boolean = java.lang.Float.isFinite(toFloat)

  override def toString = s"MiniFloat($toFloat)"

  override def equals(other: Any): Boolean = other match {
    case that: MiniFloat => this.toFloat == that.toFloat
    case _               => false
  }

  override def hashCode: Int = java.lang.Float.hashCode(toFloat)

}

object MiniFloat {

  object PositiveInfinity extends MiniFloat(Float.PositiveInfinity)
  object NegativeInfinity extends MiniFloat(Float.NegativeInfinity)
  object NaN extends MiniFloat(Float.NaN)

  final private class Finite private (significand: Int, exponent: Int)
      extends MiniFloat(significand * math.pow(Finite.base.toDouble, exponent.toDouble).toFloat)

  private[MiniFloat] object Finite {

    private[MiniFloat] val base = 2

    private val minSignificand = -2
    private val maxSignificand = 2

    private val minExponent = -1
    private val maxExponent = 2

    val allValues: List[Finite] = {
      for {
        significand <- Range.inclusive(minSignificand, maxSignificand)
        exponent <- Range.inclusive(minExponent, maxExponent)
      } yield new Finite(significand, exponent)
    }.toList.ordDistinct(Order.by[Finite, Float](_.toFloat)).sortBy(_.toFloat)

    private[MiniFloat] val allValuesAsFloats: List[Float] = allValues.map(_.toFloat)

    val zero = new Finite(0, 0)
    val max = new Finite(maxSignificand, maxExponent)
    val min = new Finite(minSignificand, maxExponent)
    val minPositive = new Finite(significand = 1, exponent = minExponent)

    /**
     * Returns `None` if the given float cannot fit in an instance of `Finite`.
     */
    def from(float: Float): Option[Finite] = {
      val exponent: Int = getExponent(float)
      val significand: Int = math.round(float / math.pow(Finite.base.toDouble, exponent.toDouble).toFloat)

      if (significand == 0 || exponent < minExponent) {
        Some(zero)
      } else if (withinBounds(significand, exponent)) {
        Some(new Finite(significand, exponent))
      } else if (exponent > maxExponent) {
        try {
          val ordersOfMagnitudeToShift = Math.subtractExact(exponent, maxExponent)

          val proposedSignificand: Int = Math.multiplyExact(
            significand,
            math.pow(base.toDouble, ordersOfMagnitudeToShift.toDouble).toInt
          )
          val proposedExponent: Int = Math.subtractExact(exponent, ordersOfMagnitudeToShift)

          if (withinBounds(proposedSignificand, proposedExponent)) {
            Some(new Finite(proposedSignificand, proposedExponent))
          } else {
            None
          }
        } catch {
          case _: ArithmeticException => None
        }
      } else {
        None
      }
    }

    private def withinBounds(significand: Int, exponent: Int): Boolean =
      (minExponent <= exponent && exponent <= maxExponent) &&
        (minSignificand <= significand && significand <= maxSignificand)

    private val floatExponentStartBit: Int = 23
    private val floatExponentLength: Int = 8
    private val floatExponentBias: Int = 127
    private val floatExponentMask: Int = ((1 << floatExponentLength) - 1) << floatExponentStartBit

    // This does the same thing as java.lang.Math.getExponent, but that method is not available in scalaJS so we have to
    // do the same thing here.
    private def getExponent(float: Float): Int =
      ((floatExponentMask & java.lang.Float.floatToIntBits(float)) >> floatExponentStartBit) - floatExponentBias

  }

  val Zero: MiniFloat = Finite.zero
  val NegativeOne: MiniFloat = MiniFloat.from(-1f)
  val One: MiniFloat = MiniFloat.from(1f)

  val MaxValue: MiniFloat = Finite.max
  val MinValue: MiniFloat = Finite.min
  val MinPositiveValue: MiniFloat = Finite.minPositive

  def allValues: List[MiniFloat] =
    List(NegativeInfinity) ++
      Finite.allValues :+
      PositiveInfinity :+
      NaN

  def from(float: Float): MiniFloat =
    float match {
      case Float.PositiveInfinity => PositiveInfinity
      case Float.NegativeInfinity => NegativeInfinity
      case f if f.isNaN           => NaN
      case _ =>
        Finite
          .from(float)
          .getOrElse {
            if (float > 0) PositiveInfinity else NegativeInfinity
          }
    }

  def from(double: Double): MiniFloat = from(double.toFloat)
  def from(int: Int): MiniFloat = from(int.toFloat)
  def from(long: Long): MiniFloat = from(long.toFloat)

  /**
   * Note that since `MiniFloat` is used primarily for tests, this `Eq` instance defines `NaN` as equal to itself. This
   * differs from the `Order` defined for `Float`.
   */
  implicit val catsLawsEqInstancesForMiniFloat: Order[MiniFloat] with Hash[MiniFloat] =
    new Order[MiniFloat] with Hash[MiniFloat] {
      override def compare(x: MiniFloat, y: MiniFloat): Int = Order[Float].compare(x.toFloat, y.toFloat)
      override def hash(x: MiniFloat): Int = Hash[Float].hash(x.toFloat)
    }

  implicit val catsLawsExhaustiveCheckForMiniInt: ExhaustiveCheck[MiniFloat] = new ExhaustiveCheck[MiniFloat] {
    override def allValues: List[MiniFloat] = MiniFloat.allValues
  }

  val miniFloatMax: CommutativeMonoid[MiniFloat] with BoundedSemilattice[MiniFloat] =
    new CommutativeMonoid[MiniFloat] with BoundedSemilattice[MiniFloat] {
      override def empty: MiniFloat = MiniFloat.NegativeInfinity
      override def combine(x: MiniFloat, y: MiniFloat): MiniFloat = if (x > y) x else y
    }

}
