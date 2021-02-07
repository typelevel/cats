package cats.tests

import cats.laws.discipline.{ExhaustiveCheck, MiniInt}
import cats.laws.discipline.MiniInt._
import cats.laws.discipline.eq._
import cats.kernel.{Eq, Order}
import cats.syntax.either._

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

  implicit protected def eqNumeric[A: Eq: ExhaustiveCheck]: Eq[Numeric[A]] = {

    // These allow us to capture the cases where operations on Numeric throw (eg when causing an overflow). These are
    // represented by `None` and allow us to compare two Numeric instances, verifying that when one throws, the other
    // also throws.
    def makeUnaryFnSafe[X, R](f: X => R): X => Option[R] =
      x => Either.catchOnly[IllegalArgumentException](f(x)).toOption
    def makeBinaryFnSafe[X, Y, R](f: (X, Y) => R): (X, Y) => Option[R] =
      (x, y) => Either.catchOnly[IllegalArgumentException](f(x, y)).toOption

    Eq.by { numeric =>
      val fromMiniInt: MiniInt => Option[A] = makeUnaryFnSafe(miniInt => numeric.fromInt(miniInt.toInt))

      (
        makeBinaryFnSafe(numeric.compare),
        makeBinaryFnSafe(numeric.plus),
        makeBinaryFnSafe(numeric.minus),
        makeBinaryFnSafe(numeric.times),
        makeUnaryFnSafe(numeric.negate),
        fromMiniInt,
        makeUnaryFnSafe(numeric.toInt),
        makeUnaryFnSafe(numeric.toLong),
        makeUnaryFnSafe(numeric.toFloat),
        makeUnaryFnSafe(numeric.toDouble)
      )
    }
  }
}
