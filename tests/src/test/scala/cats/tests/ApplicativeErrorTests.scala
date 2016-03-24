package cats
package tests

import cats.data.{Xor, XorT}

class ApplicativeErrorCheck extends CatsSuite {

  type ErrorOr[A] = String Xor A

  val failed: String Xor Int =
    "Badness".raiseError[ErrorOr, Int]

  test("raiseError syntax creates an Xor with the correct type parameters") {
    failed should === ("Badness".left[Int])
  }

  test("handleError syntax transforms an error to a success") {
    failed.handleError(error => error.length) should === (7.right)
  }

  test("handleErrorWith transforms an error to a success") {
    failed.handleErrorWith(error => error.length.right) should === (7.right)
  }

  test("attempt syntax creates a wrapped Xor") {
    failed.attempt should === ("Badness".left.right)
  }

  test("attemptT syntax creates an XorT") {
    type ErrorOrT[A] = XorT[ErrorOr, String, A]
    failed.attemptT should === (XorT[ErrorOr, String, Int](failed.right))
  }

  test("recover syntax transforms an error to a success") {
    failed.recover { case error => error.length } should === (7.right)
  }

  test("recoverWith transforms an error to a success") {
    failed.recoverWith { case error => error.length.right } should === (7.right)
  }

}