package cats.tests

import cats._
import cats.kernel.laws.discipline.EqTests
import cats.data.EitherK
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._

class EitherKSuite extends CatsSuite {

  checkAll("EitherK[Option, Option, ?]",
           TraverseTests[EitherK[Option, Option, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[EitherK[Option, Option, ?]]", SerializableTests.serializable(Traverse[EitherK[Option, Option, ?]]))

  {
    implicit val foldable = EitherK.catsDataFoldableForEitherK[Option, Option]
    checkAll("EitherK[Option, Option, ?]", FoldableTests[EitherK[Option, Option, ?]].foldable[Int, Int])
    checkAll("Foldable[EitherK[Option, Option, ?]]",
             SerializableTests.serializable(Foldable[EitherK[Option, Option, ?]]))
  }

  checkAll("EitherK[Eval, Eval, ?]", ComonadTests[EitherK[Eval, Eval, ?]].comonad[Int, Int, Int])
  checkAll("Comonad[EitherK[Eval, Eval, ?]]", SerializableTests.serializable(Comonad[EitherK[Eval, Eval, ?]]))

  {
    implicit val coflatMap = EitherK.catsDataCoflatMapForEitherK[Eval, Eval]
    checkAll("EitherK[Eval, Eval, ?]", CoflatMapTests[EitherK[Eval, Eval, ?]].coflatMap[Int, Int, Int])
    checkAll("CoflatMap[EitherK[Eval, Eval, ?]]", SerializableTests.serializable(CoflatMap[EitherK[Eval, Eval, ?]]))
  }

  checkAll("EitherK[Option, Option, Int]", EqTests[EitherK[Option, Option, Int]].eqv)
  checkAll("Eq[EitherK[Option, Option, Int]]", SerializableTests.serializable(Eq[EitherK[Option, Option, Int]]))

  checkAll("EitherK[Show, Show, ?]", ContravariantTests[EitherK[Show, Show, ?]].contravariant[MiniInt, Int, Boolean])
  checkAll("Contravariant[EitherK[Show, Show, ?]]",
           SerializableTests.serializable(Contravariant[EitherK[Show, Show, ?]]))

  test("double swap is identity") {
    forAll { (x: EitherK[Option, Option, Int]) =>
      x.swap.swap should ===(x)
    }
  }

  test("swap negates isLeft/isRight") {
    forAll { (x: EitherK[Option, Option, Int]) =>
      x.isLeft should !==(x.swap.isLeft)
      x.isRight should !==(x.swap.isRight)
    }
  }

  test("isLeft consistent with isRight") {
    forAll { (x: EitherK[Option, Option, Int]) =>
      x.isLeft should !==(x.isRight)
    }
  }

  test("toValidated + toEither is identity") {
    forAll { (x: EitherK[Option, List, Int]) =>
      x.toValidated.toEither should ===(x.run)
    }
  }
}
