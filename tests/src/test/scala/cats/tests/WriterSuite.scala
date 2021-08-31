package cats.tests

import cats.data.Writer
import cats.syntax.applicative._
import cats.syntax.writer._
import cats.syntax.eq._
import org.scalacheck.Prop._

class WriterSuite extends CatsSuite {
  test("pure syntax creates a writer with an empty log") {
    forAll { (result: String) =>
      type Logged[A] = Writer[List[Int], A]
      assert(result.pure[Logged] === (Writer(List.empty[Int], result)))
    }
  }

  test("tell syntax creates a writer with a unit result") {
    forAll { (log: List[Int]) =>
      assert(log.tell === (Writer(log, ())))
    }
  }

  test("writer syntax creates a writer with the specified result and log") {
    forAll { (result: String, log: List[Int]) =>
      assert(result.writer(log) === (Writer(log, result)))
    }
  }

  test("catsDataCommutativeMonadForWriterT and catsDataTraverseForWriterTId instances are not ambiguous") {
    import cats.Functor
    Functor[Writer[Int, *]]
  }
}
