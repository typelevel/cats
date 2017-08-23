package cats


import cats.data.{EitherT, Nested, OptionT, Validated}
import cats.tests.CatsSuite
import cats.laws.discipline.{ParallelTests => ParallelTypeclassTests}
import org.scalacheck.Arbitrary

class ParallelTests extends CatsSuite {


  test("ParTraversing Either should accumulate errors") {
    forAll { es: List[Either[String, Int]] =>
      val lefts = es.collect {
        case Left(e) => e
      }.foldMap(identity)

      (es.parSequence.fold(identity, i => Monoid[String].empty)) should === (lefts)
    }
  }

  test("ParTraverse identity should be equivalent to parSequence") {
    forAll { es: List[Either[String, Int]] =>
      (es.parTraverse(identity)) should === (es.parSequence)
    }
  }

  test("ParTraverse_ identity should be equivalent to parSequence_") {
    forAll { es: Set[Either[String, Int]] =>
      (Parallel.parTraverse_(es)(identity)) should === (Parallel.parSequence_(es))
    }
  }

  test("parAp2 accumulates errors in order") {
    val plus = (_: Int) + (_: Int)
    val rightPlus: Either[String, (Int, Int) => Int] = Right(plus)
    Parallel.parAp2(rightPlus)("Hello".asLeft, "World".asLeft) should === (Left("HelloWorld"))
  }

  checkAll("Parallel[Either[String, ?], Validated[String, ?]]", ParallelTypeclassTests[Either[String, ?], Validated[String, ?], Int].parallel)

  {
    implicit val arbO: Arbitrary[OptionT[Either[String, ?], Int]] = cats.laws.discipline.arbitrary.catsLawsArbitraryForOptionT
    implicit val arbE: Arbitrary[EitherT[Either[String, ?], String, Int]] = cats.laws.discipline.arbitrary.catsLawsArbitraryForEitherT

    checkAll("Parallel[OptionT[M, ?], Nested[F, Option, ?]]", ParallelTypeclassTests[OptionT[Either[String, ?], ?], Nested[Validated[String, ?], Option, ?], Int].parallel)
    checkAll("Parallel[EitherT[M, E, ?], Nested[F, Validated[E, ?], ?]]", ParallelTypeclassTests[EitherT[Either[String, ?], String, ?], Nested[Validated[String, ?], Validated[String, ?], ?], Int].parallel)
  }
}
