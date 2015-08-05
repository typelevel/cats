package cats
package tests

import org.scalactic.anyvals.{PosZDouble, PosInt}

private[tests] object Platform {

  // Override defaults to mimick scalatest 2.2.5 values
  val minSuccessful = PosInt(100)
  val maxDiscardedFactor = PosZDouble(5.0)

  trait UltraSlowCatsSuite extends CatsSuite {}
}
