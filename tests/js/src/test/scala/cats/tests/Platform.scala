package cats
package tests

import org.scalactic.anyvals.{PosZDouble, PosInt}

private[tests] object Platform {

  // Override defaults to mimick scalatest 2.2.5 values
  val minSuccessful = PosInt(10)
  val maxDiscardedFactor = PosZDouble(50.0)

  trait UltraSlowCatsSuite extends CatsSuite {
    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(maxSize = 1, minSuccessful = 1)
  }
}
