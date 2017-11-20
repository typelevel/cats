package cats
package tests

class MonadErrorSuite extends CatsSuite {

  val successful: Option[Int] = 42.some
  val failed: Option[Int] = None

  test("ensure raises an error if the predicate fails") {
    successful.ensure(())(i => false) should === (None)
  }

  test("ensure returns the successful value if the predicate succeeds") {
    successful.ensure(())(i => true) should === (successful)
  }

  test("ensure returns the failure, when applied to a failure") {
    failed.ensure(())(i => false) should === (failed)
    failed.ensure(())(i => true) should === (failed)
  }

  test("ensureOr raises an error if the predicate fails") {
    successful.ensureOr(_ => ())(_ => false) should === (None)
  }

  test("ensureOr returns the successful value if the predicate succeeds") {
    successful.ensureOr(_ => ())(_ => true) should === (successful)
  }

  test("ensureOr returns the failure, when applied to a failure") {
    failed.ensureOr(_ => ())(_ => false) should === (failed)
    failed.ensureOr(_ => ())(_ => true) should === (failed)
  }


}
