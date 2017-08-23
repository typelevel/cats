package cats


import cats.data.Validated
import cats.tests.CatsSuite
import cats.laws.discipline.{ParallelTests => ParallelTypeclassTests}

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

}
