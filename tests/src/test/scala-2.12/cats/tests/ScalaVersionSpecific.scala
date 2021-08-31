package cats.tests

import cats.laws.discipline.{ExhaustiveCheck, MiniInt}
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

  implicit protected def eqNumeric[A: Eq: ExhaustiveCheck]: Eq[Numeric[A]] = Eq.by { numeric =>
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
}
