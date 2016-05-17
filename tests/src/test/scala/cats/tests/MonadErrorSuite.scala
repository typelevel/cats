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

}
