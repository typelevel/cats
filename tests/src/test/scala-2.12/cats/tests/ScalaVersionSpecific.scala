package cats.tests

import cats.laws.discipline.{ExhaustiveCheck, MiniInt}
import cats.laws.discipline.MiniInt._
import cats.laws.discipline.eq._
import cats.kernel.{Eq, Order}
import org.scalacheck.{Arbitrary, Gen}

trait ScalaVersionSpecificFoldableSuite
trait ScalaVersionSpecificParallelSuite
trait ScalaVersionSpecificRegressionSuite
trait ScalaVersionSpecificTraverseSuite

trait ScalaVersionSpecificAlgebraInvariantSuite {
  implicit protected val arbNumericMiniInt: Arbitrary[Numeric[MiniInt]] = Arbitrary {
    Gen.const {
      new Numeric[MiniInt] {
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
      }
    }
  }

  implicit protected def eqNumeric[A: Eq: ExhaustiveCheck]: Eq[Numeric[A]] = Eq.by { numeric =>
    val fromInt = numeric.fromInt _

    (
      numeric.compare _,
      numeric.plus _,
      numeric.minus _,
      numeric.times _,
      numeric.negate _,
      fromInt.compose((_: MiniInt).toInt),
      numeric.toInt _,
      numeric.toLong _,
      numeric.toFloat _,
      numeric.toDouble _,
    )
  }
}
