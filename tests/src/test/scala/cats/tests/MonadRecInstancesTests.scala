package cats
package tests

import cats.data.{OptionT, Xor, XorT}

class MonadRecInstancesTests extends CatsSuite {
  def tailRecMStackSafety[M[_]](implicit M: MonadRec[M], Eq: Eq[M[Int]]): Unit = {
    val n = 50000
    val res = M.tailRecM(0)(i => M.pure(if(i < n) Xor.Left(i + 1) else Xor.Right(i)))
    res should === (M.pure(n))
  }

  test("tailRecM stack-safety for Id") {
    tailRecMStackSafety[Id]
  }

  test("tailRecM stack-safety for Option") {
    tailRecMStackSafety[Option]
  }

  test("tailRecM stack-safety for OptionT") {
    tailRecMStackSafety[OptionT[Option, ?]]
  }

  test("tailRecM stack-safety for Either") {
    tailRecMStackSafety[Either[String, ?]]
  }

  test("tailRecM stack-safety for Xor") {
    tailRecMStackSafety[String Xor ?]
  }

  test("tailRecM stack-safety for XorT") {
    tailRecMStackSafety[XorT[Option, String, ?]]
  }

  test("tailRecM stack-safety for List") {
    tailRecMStackSafety[List]
  }

}
