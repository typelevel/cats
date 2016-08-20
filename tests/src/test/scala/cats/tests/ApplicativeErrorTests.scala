package cats
package tests

import cats.data.{Xor, XorT}

class ApplicativeErrorCheck extends CatsSuite {
  val failed: Option[Int] =
    (()).raiseError[Option, Int]

  test("raiseError syntax creates an Option with the correct value") {
    failed should === (None: Option[Int])
  }

  test("handleError syntax transforms an error to a success") {
    failed.handleError(_ => 7) should === (Some(7))
  }

  test("handleErrorWith transforms an error to a success") {
    failed.handleErrorWith(_ => Some(7)) should === (Some(7))
  }

  test("attempt syntax creates a wrapped Xor") {
    failed.attempt should === (Option(Xor.Left(())))
  }

  test("attemptT syntax creates an XorT") {
    failed.attemptT should === (XorT[Option, Unit, Int](Option(Xor.Left(()))))
  }

  test("recover syntax transforms an error to a success") {
    failed.recover { case _ => 7 } should === (Some(7))
  }

  test("recoverWith transforms an error to a success") {
    failed.recoverWith { case _ => Some(7) } should === (Some(7))
  }


}
