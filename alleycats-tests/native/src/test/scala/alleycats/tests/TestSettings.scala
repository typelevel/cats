package alleycats.tests

import org.scalacheck.Test.Parameters

trait TestSettings {

  lazy val checkConfiguration: Parameters =
    Parameters.default
      .withMinSuccessfulTests(50)
      .withMaxDiscardRatio(5.0f)
      .withMaxSize(10)
      .withMinSize(0)
      .withWorkers(1)
}
