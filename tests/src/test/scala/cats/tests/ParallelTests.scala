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

  checkAll("Parallel[Either[String, ?], Validated[String, ?]]", ParallelTypeclassTests[Either[String, ?], Validated[String, ?], Int].parallel)

}
