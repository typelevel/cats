package cats.tests

import cats.data.Writer
import cats.instances.all._
import cats.syntax.applicative._
import cats.syntax.writer._

class WriterSuite extends CatsSuite {
  test("pure syntax creates a writer with an empty log") {
    forAll { (result: String) =>
      type Logged[A] = Writer[List[Int], A]
      result.pure[Logged] should ===(Writer(List.empty[Int], result))
    }
  }

  test("tell syntax creates a writer with a unit result") {
    forAll { (log: List[Int]) =>
      log.tell should ===(Writer(log, ()))
    }
  }

  test("writer syntax creates a writer with the specified result and log") {
    forAll { (result: String, log: List[Int]) =>
      result.writer(log) should ===(Writer(log, result))
    }
  }
}
