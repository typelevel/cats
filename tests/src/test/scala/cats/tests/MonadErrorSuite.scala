package cats
package tests

import cats.data.Xor

class MonadErrorSuite extends CatsSuite {

  type ErrorOr[A] = String Xor A

  val successful: ErrorOr[Int] = 42.right
  val failed: ErrorOr[Int] = "Oops".left

  test("ensure raises an error if the predicate fails") {
    monadErrorSyntax(successful).ensure("Error")(i => false) should === ("Error".left)
  }

  test("ensure returns the successful value if the predicate succeeds") {
    monadErrorSyntax(successful).ensure("Error")(i => true) should === (successful)
  }

  test("ensure returns the failure, when applied to a failure") {
    monadErrorSyntax(failed).ensure("Error")(i => false) should === (failed)
    monadErrorSyntax(failed).ensure("Error")(i => true) should === (failed)
  }

}
