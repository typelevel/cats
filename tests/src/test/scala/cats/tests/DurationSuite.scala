package cats.tests

import cats.Show
import cats.laws.discipline.SerializableTests
import scala.concurrent.duration.{Duration, DurationInt}
import cats.syntax.eq._

class DurationSuite extends CatsSuite {
  checkAll("Show[Duration]", SerializableTests.serializable(Show[Duration]))

  test("show works for FiniteDuration") {
    assert(Show[Duration].show(23.minutes) === ("23 minutes"))
  }

  test("show works for non-finite durations") {
    assert(Show[Duration].show(Duration.Inf) === ("Duration.Inf"))
    assert(Show[Duration].show(Duration.MinusInf) === ("Duration.MinusInf"))
    assert(Show[Duration].show(Duration.Undefined) === ("Duration.Undefined"))
  }
}
