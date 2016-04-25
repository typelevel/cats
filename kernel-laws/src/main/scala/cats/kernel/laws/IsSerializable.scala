package cats.kernel.laws

import catalysts.Platform

import scala.util.DynamicVariable

/**
 * Object with a dynamic variable that allows users to skip the
 * serialization tests for certain instances.
 */
private[laws] object IsSerializable {
  val runTests = new DynamicVariable[Boolean](true)
  def apply(): Boolean = (!Platform.isJs) && runTests.value
}
