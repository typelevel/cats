package fix
package to1_0_0

import cats.implicits._
import cats.arrow.Arrow

object RemoveSplitTests {
  val toLong: Int => Long = _.toLong
  val toDouble: Float => Double = _.toDouble
  val f: ((Int, Float)) => (Long, Double) =
    Arrow[Function1].split(toLong, toDouble)
  f((3, 4.0f))

  {
    import cats.syntax.arrow._
    toLong split toDouble
  }
}
