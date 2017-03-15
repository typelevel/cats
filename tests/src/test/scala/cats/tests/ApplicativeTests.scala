package cats
package tests

import cats.Applicative


class ApplicativeCheck extends CatsSuite {

  test("replicateA creates a List of 'n' copies of given Applicative 'fa'") {

    val A = Applicative[Option]
    val fa = A.pure(1)
    A.replicateA(5, fa) should === (Some(List(1,1,1,1,1)))

  }

  test("optional") {
    val error  : Either[String, Int] = Left("error")
    val success: Either[String, Int] = Right(42)

    val A = Applicative[Either[String, ?]]

    A.optional(error)   should === (Right(None))
    A.optional(success) should === (Right(Some(42)))
  }
}