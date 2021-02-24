package cats.tests

import cats.laws.discipline.{ExhaustiveCheck, MiniInt, MiniFloat}
import cats.laws.discipline.MiniInt._
import cats.laws.discipline.eq._
import cats.kernel.{Eq, Order}

trait ScalaVersionSpecificFoldableSuite
trait ScalaVersionSpecificParallelSuite
trait ScalaVersionSpecificRegressionSuite
trait ScalaVersionSpecificTraverseSuite

trait ScalaVersionSpecificAlgebraInvariantSuite {
  protected val integralForMiniInt: Integral[MiniInt] = new Integral[MiniInt] {
    def compare(x: MiniInt, y: MiniInt): Int = Order[MiniInt].compare(x, y)
    def plus(x: MiniInt, y: MiniInt): MiniInt = x + y
    def minus(x: MiniInt, y: MiniInt): MiniInt = x + (-y)
    def times(x: MiniInt, y: MiniInt): MiniInt = x * y
    def negate(x: MiniInt): MiniInt = -x
    def fromInt(x: Int): MiniInt = MiniInt.unsafeFromInt(x)
    def toInt(x: MiniInt): Int = x.toInt
    def toLong(x: MiniInt): Long = x.toInt.toLong
    def toFloat(x: MiniInt): Float = x.toInt.toFloat
    def toDouble(x: MiniInt): Double = x.toInt.toDouble
    def quot(x: MiniInt, y: MiniInt): MiniInt = MiniInt.unsafeFromInt(x.toInt / y.toInt)
    def rem(x: MiniInt, y: MiniInt): MiniInt = MiniInt.unsafeFromInt(x.toInt % y.toInt)
  }

  protected val fractionalForMiniFloat: Fractional[MiniFloat] = new Fractional[MiniFloat] {
    def compare(x: MiniFloat, y: MiniFloat): Int = Order[MiniFloat].compare(x, y)
    def plus(x: MiniFloat, y: MiniFloat): MiniFloat = x + y
    def minus(x: MiniFloat, y: MiniFloat): MiniFloat = x - y
    def times(x: MiniFloat, y: MiniFloat): MiniFloat = x * y
    def negate(x: MiniFloat): MiniFloat = -x
    def fromInt(x: Int): MiniFloat = MiniFloat.from(x)
    def toInt(x: MiniFloat): Int = x.toInt
    def toLong(x: MiniFloat): Long = x.toLong
    def toFloat(x: MiniFloat): Float = x.toFloat
    def toDouble(x: MiniFloat): Double = x.toDouble
    def div(x: MiniFloat, y: MiniFloat): MiniFloat = x / y
  }

  protected def versionSpecificNumericEq[A: Eq: ExhaustiveCheck]: Eq[Numeric[A]] = Eq.allEqual

}
