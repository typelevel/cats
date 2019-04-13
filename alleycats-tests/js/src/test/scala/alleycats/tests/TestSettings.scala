package alleycats
package tests

import org.scalactic.anyvals.{PosInt, PosZDouble, PosZInt}
import org.scalatest.Matchers
import org.scalatest.prop.Configuration

trait TestSettings extends Configuration with Matchers {

  lazy val checkConfiguration: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = PosInt(5),
      maxDiscardedFactor = PosZDouble(50.0),
      minSize = PosZInt(0),
      sizeRange = PosZInt(5),
      workers = PosInt(1)
    )
}
