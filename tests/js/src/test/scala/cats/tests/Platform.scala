package cats
package tests

private[tests] object Platform {

  trait UltraSlowCatsSuite extends CatsSuite {
    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(maxSize = 1, minSuccessful = 1)
  }
}
