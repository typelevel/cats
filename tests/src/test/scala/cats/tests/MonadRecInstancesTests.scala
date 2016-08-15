package cats
package tests

import cats.data.{
  Cokleisli,
  EitherT,
  IdT,
  Ior,
  Kleisli,
  NonEmptyList,
  NonEmptyVector,
  OneAnd,
  OptionT,
  StateT,
  Xor,
  XorT,
  WriterT
}

class MonadRecInstancesTests extends CatsSuite {
  def tailRecMStackSafety[M[_]: RecursiveTailRecM](implicit M: Monad[M], Eq: Eq[M[Int]]): Unit = {
    val n = 50000
    val res = M.tailRecM(0)(i => M.pure(if (i < n) Either.left(i + 1) else Either.right(i)))
    res should === (M.pure(n))
  }

  test("tailRecM stack-safety for Cokleisli[Option, Int, ?]") {
    implicit val eq: Eq[Cokleisli[Option, Int, Int]] = new Eq[Cokleisli[Option, Int, Int]] {
      def eqv(a: Cokleisli[Option, Int, Int], b: Cokleisli[Option, Int, Int]) =
        a.run(None) == b.run(None)
    }
    tailRecMStackSafety[Cokleisli[Option, Int, ?]]
  }

  test("tailRecM stack-safety for Kleisli[Option, Int, ?]") {
    implicit val eq: Eq[Kleisli[Option, Int, Int]] = new Eq[Kleisli[Option, Int, Int]] {
      def eqv(a: Kleisli[Option, Int, Int], b: Kleisli[Option, Int, Int]) =
        a.run(0) == b.run(0)
    }
    tailRecMStackSafety[Kleisli[Option, Int, ?]]
  }

  test("tailRecM stack-safety for Id") {
    tailRecMStackSafety[Id]
  }

  test("tailRecM stack-safety for IdT[Option, ?]") {
    tailRecMStackSafety[IdT[Option, ?]]
  }

  test("tailRecM stack-safety for Option") {
    tailRecMStackSafety[Option]
  }

  test("tailRecM stack-safety for OptionT") {
    tailRecMStackSafety[OptionT[Option, ?]]
  }

  test("tailRecM stack-safety for OneAnd[Stream, ?]") {
    tailRecMStackSafety[OneAnd[Stream, ?]]
  }

  test("tailRecM stack-safety for Either") {
    tailRecMStackSafety[Either[String, ?]]
  }

  test("tailRecM stack-safety for NonEmptyList") {
    tailRecMStackSafety[NonEmptyList]
  }

  test("tailRecM stack-safety for NonEmptyVector") {
    tailRecMStackSafety[NonEmptyVector]
  }

  test("tailRecM stack-safety for Xor") {
    tailRecMStackSafety[String Xor ?]
  }

  test("tailRecM stack-safety for XorT") {
    tailRecMStackSafety[XorT[Option, String, ?]]
  }

  test("tailRecM stack-safety for EitherT") {
    tailRecMStackSafety[EitherT[Option, String, ?]]
  }

  test("tailRecM stack-safety for Ior") {
    tailRecMStackSafety[Int Ior ?]
  }

  test("tailRecM stack-safety for List") {
    tailRecMStackSafety[List]
  }

  test("tailRecM stack-safety for Stream") {
    tailRecMStackSafety[Stream]
  }

  test("tailRecM stack-safety for Vector") {
    tailRecMStackSafety[Vector]
  }

  test("tailRecM stack-safety for Eval") {
    tailRecMStackSafety[Eval]
  }

  test("tailRecM stack-safety for StateT") {
    import StateTTests._ // import implicit Eq[StateT[...]]
    tailRecMStackSafety[StateT[Option, Int, ?]]
  }

  test("tailRecM stack-safety for WriterT[Option, Int, ?]") {
    tailRecMStackSafety[WriterT[Option, Int, ?]]
  }
}
