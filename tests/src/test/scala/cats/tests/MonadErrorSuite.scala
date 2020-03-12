package cats.tests

import cats.instances.all._
import cats.kernel.Eq
import cats.syntax.applicativeError._
import cats.syntax.monadError._
import scala.util.{Failure, Success, Try}

class MonadErrorSuite extends CatsSuite {

  implicit val eqThrow: Eq[Throwable] = Eq.fromUniversalEquals

  val successful: Try[Int] = Success(42)
  val failedValue: Throwable = new IllegalArgumentException("default failure")
  val otherValue: Throwable = new IllegalStateException("other failure")
  val failed: Try[Int] = Failure(failedValue)

  test("ensure raises an error if the predicate fails") {
    successful.ensure(failedValue)(_ => false) should ===(failed)
  }

  test("ensure returns the successful value if the predicate succeeds") {
    successful.ensure(failedValue)(_ => true) should ===(successful)
  }

  test("ensure returns the original failure, when applied to a failure") {
    failed.ensure(otherValue)(_ => false) should ===(failed)
    failed.ensure(otherValue)(_ => true) should ===(failed)
  }

  test("ensureOr raises an error if the predicate fails") {
    successful.ensureOr(_ => failedValue)(_ => false) should ===(failed)
  }

  test("ensureOr returns the successful value if the predicate succeeds") {
    successful.ensureOr(_ => failedValue)(_ => true) should ===(successful)
  }

  test("ensureOr returns the original failure, when applied to a failure") {
    failed.ensureOr(_ => otherValue)(_ => false) should ===(failed)
    failed.ensureOr(_ => otherValue)(_ => true) should ===(failed)
  }

  test("reject returns the successful value if the partial function is not defined") {
    successful.reject {
      case i if i < 0 => failedValue
    } should ===(successful)
  }

  test("reject returns the original failure, when applied to a failure") {
    failed.reject {
      case i if i < 0 => otherValue
    } should ===(failed)
  }

  test("reject raises an error if the partial function is defined") {
    successful.reject {
      case i if i > 0 => failedValue
    } should ===(failed)
  }

  test("rethrow returns the failure, when applied to a Left of a failure") {
    failed.attempt.rethrow should ===(failed)
  }

  test("rethrow returns the successful value, when applied to a Right of a successful value") {
    successful.attempt.rethrow should ===(successful)
  }

  test("rethrow returns the failure, when applied to a Left of a specialized failure") {
    failed.attempt.asInstanceOf[Try[Either[IllegalArgumentException, Int]]].rethrow should ===(failed)
  }

  test("rethrow returns the successful value, when applied to a Right of a specialized successful value") {
    successful.attempt.asInstanceOf[Try[Either[IllegalArgumentException, Int]]].rethrow should ===(successful)
  }
}
