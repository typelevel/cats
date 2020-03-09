package cats.tests

import cats.Show
import cats.instances.all._
import cats.laws.discipline.SerializableTests
import scala.concurrent.duration.{Duration, DurationInt}

class DurationSuite extends CatsSuite {
  checkAll("Show[Duration]", SerializableTests.serializable(Show[Duration]))

  test("show works for FiniteDuration") {
    Show[Duration].show(23.minutes) should ===("23 minutes")
  }

  test("show works for non-finite durations") {
    Show[Duration].show(Duration.Inf) should ===("Duration.Inf")
    Show[Duration].show(Duration.MinusInf) should ===("Duration.MinusInf")
    Show[Duration].show(Duration.Undefined) should ===("Duration.Undefined")
  }
}
