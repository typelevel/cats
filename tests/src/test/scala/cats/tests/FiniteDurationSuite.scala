package cats.tests

import cats.Show
import cats.laws.discipline.SerializableTests
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import cats.syntax.eq._

class FiniteDurationSuite extends CatsSuite {
  checkAll("Show[FiniteDuration]", SerializableTests.serializable(Show[FiniteDuration]))

  test("show works for FiniteDuration") {
    assert(Show[FiniteDuration].show(23.minutes) === "23 minutes")
    assert(Show[FiniteDuration].show(10.seconds) === "10 seconds")
  }
}
