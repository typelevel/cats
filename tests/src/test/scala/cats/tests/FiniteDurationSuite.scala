package cats
package tests

import cats.laws.discipline.SerializableTests

import scala.concurrent.duration.{DurationInt, FiniteDuration}

class FiniteDurationSuite extends CatsSuite {
  checkAll("Show[FiniteDuration]", SerializableTests.serializable(Show[FiniteDuration]))

  test("show works for FiniteDuration") {
    Show[FiniteDuration].show(23.minutes) should ===("23 minutes")
    Show[FiniteDuration].show(10.seconds) should ===("10 seconds")
  }
}
