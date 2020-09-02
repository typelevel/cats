/*
rule = "scala:fix.v1_0_0.RemoveSplit"
 */
package fix
package to1_0_0

import cats.implicits._
import cats.arrow.Split

object RemoveSplitTests {
  val toLong: Int => Long = _.toLong
  val toDouble: Float => Double = _.toDouble
  val f: ((Int, Float)) => (Long, Double) =
    Split[Function1].split(toLong, toDouble)
  f((3, 4.0f))

  {
    import cats.syntax.split._
    toLong split toDouble
  }
}
