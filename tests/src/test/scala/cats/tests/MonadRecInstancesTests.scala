package cats
package tests

import cats.data.{OptionT, StateT, Xor, XorT}

class MonadRecInstancesTests extends CatsSuite {
  def tailRecMStackSafety[M[_]](implicit M: MonadRec[M], Eq: Eq[M[Int]]): Unit = {
    val n = 50000
    val res = M.tailRecM(0)(i => M.pure(if (i < n) Xor.Left(i + 1) else Xor.Right(i)))
    res should === (M.pure(n))
  }

  slowTest("tailRecM stack-safety for Id") {
    tailRecMStackSafety[Id]
  }

  slowTest("tailRecM stack-safety for Option") {
    tailRecMStackSafety[Option]
  }

  slowTest("tailRecM stack-safety for OptionT") {
    tailRecMStackSafety[OptionT[Option, ?]]
  }

  slowTest("tailRecM stack-safety for Either") {
    tailRecMStackSafety[Either[String, ?]]
  }

  slowTest("tailRecM stack-safety for Xor") {
    tailRecMStackSafety[String Xor ?]
  }

  slowTest("tailRecM stack-safety for XorT") {
    tailRecMStackSafety[XorT[Option, String, ?]]
  }

  slowTest("tailRecM stack-safety for List") {
    tailRecMStackSafety[List]
  }

  slowTest("tailRecM stack-safety for Eval") {
    tailRecMStackSafety[Eval]
  }

  slowTest("tailRecM stack-safety for StateT") {
    import StateTTests._ // import implicit Eq[StateT[...]]
    tailRecMStackSafety[StateT[Option, Int, ?]]
  }

}
