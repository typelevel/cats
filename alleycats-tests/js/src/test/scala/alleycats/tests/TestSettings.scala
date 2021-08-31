package alleycats.tests

import org.scalacheck.Test

trait TestSettings {
  lazy val checkConfiguration: Test.Parameters =
    Test.Parameters.default
      .withMinSuccessfulTests(5)
      .withMaxDiscardRatio(50.0f)
      .withMinSize(0)
      .withWorkers(1)
}
