package cats.tests

import cats.kernel.Eq
import cats.syntax.applicativeError._
import cats.syntax.monadError._
import scala.util.{Failure, Success, Try}
import cats.syntax.eq._

class MonadErrorSuite extends CatsSuite {

  implicit val eqThrow: Eq[Throwable] = Eq.fromUniversalEquals

  val successful: Try[Int] = Success(42)
  val failedValue: Throwable = new IllegalArgumentException("default failure")
  val otherValue: Throwable = new IllegalStateException("other failure")
  val failed: Try[Int] = Failure(failedValue)

  test("ensure raises an error if the predicate fails") {
    assert(successful.ensure(failedValue)(_ => false) === failed)
  }

  test("ensure returns the successful value if the predicate succeeds") {
    assert(successful.ensure(failedValue)(_ => true) === successful)
  }

  test("ensure returns the original failure, when applied to a failure") {
    assert(failed.ensure(otherValue)(_ => false) === failed)
    assert(failed.ensure(otherValue)(_ => true) === failed)
  }

  test("ensureOr raises an error if the predicate fails") {
    assert(successful.ensureOr(_ => failedValue)(_ => false) === failed)
  }

  test("ensureOr returns the successful value if the predicate succeeds") {
    assert(successful.ensureOr(_ => failedValue)(_ => true) === successful)
  }

  test("ensureOr returns the original failure, when applied to a failure") {
    assert(failed.ensureOr(_ => otherValue)(_ => false) === failed)
    assert(failed.ensureOr(_ => otherValue)(_ => true) === failed)
  }

  test("reject returns the successful value if the partial function is not defined") {
    assert(successful.reject {
      case i if i < 0 => failedValue
    } === successful)
  }

  test("reject returns the original failure, when applied to a failure") {
    assert(failed.reject {
      case i if i < 0 => otherValue
    } === failed)
  }

  test("reject raises an error if the partial function is defined") {
    assert(successful.reject {
      case i if i > 0 => failedValue
    } === failed)
  }

  test("rethrow returns the failure, when applied to a Left of a failure") {
    assert(failed.attempt.rethrow === failed)
  }

  test("rethrow returns the successful value, when applied to a Right of a successful value") {
    assert(successful.attempt.rethrow === successful)
  }

  test("rethrow returns the failure, when applied to a Left of a specialized failure") {
    assert(failed.attempt.asInstanceOf[Try[Either[IllegalArgumentException, Int]]].rethrow === failed)
  }

  test("rethrow returns the successful value, when applied to a Right of a specialized successful value") {
    assert(successful.attempt.asInstanceOf[Try[Either[IllegalArgumentException, Int]]].rethrow === successful)
  }
}
