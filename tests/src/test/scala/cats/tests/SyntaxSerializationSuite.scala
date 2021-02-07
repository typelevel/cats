package cats.tests

import cats.laws.discipline.SerializableTests
import cats.syntax.either._

/**
 * Test that our syntax implicits are serializable.
 */
class SyntaxSerializationSuite extends CatsSuite {
  checkAll(
    "Tuple3SemigroupalOps[Option, Boolean, Int, Long]",
    SerializableTests.serializable(
      cats.syntax.all.catsSyntaxTuple3Semigroupal[Option, Boolean, Int, Long]((None, None, None))
    )
  )

  checkAll("SemigroupalOps[Option, Int]",
           SerializableTests.serializable(cats.syntax.all.catsSyntaxSemigroupal[Option, Int](None))
  )

  checkAll(
    "Tuple3ParallelOps[Either[String, ?], Boolean, Int, Long]",
    SerializableTests.serializable(
      cats.syntax.all.catsSyntaxTuple3Parallel(("a".asLeft[Boolean], "b".asLeft[Int], "c".asLeft[Long]))
    )
  )
}
