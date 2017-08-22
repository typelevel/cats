package cats


import cats.data.Validated
import cats.tests.CatsSuite
import cats.laws.discipline.{ParallelTests => ParallelTypeclassTests}

class ParallelTests extends CatsSuite {

  checkAll("Parallel[Either[String, ?], Validated[String, ?]]", ParallelTypeclassTests[Either[String, ?], Validated[String, ?]].parallel)
}
