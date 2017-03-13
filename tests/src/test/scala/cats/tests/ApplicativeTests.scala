package cats
package tests

import cats.Applicative


class ApplicativeCheck extends CatsSuite {

  test("replicateA creates a List of 'n' copies of given Applicative 'fa'") {

    val A = Applicative[Option]
    val fa = A.pure(1)
    A.replicateA(5, fa) should === (Some(List(1,1,1,1,1)))

  }
}