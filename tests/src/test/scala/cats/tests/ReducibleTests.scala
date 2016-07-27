package cats
package tests

import org.scalacheck.Arbitrary

class ReducibleTestsAdditional extends CatsSuite {

  test("Reducible[NonEmptyList].reduceLeftM stack safety") {
    def nonzero(acc: Long, x: Long): Option[Long] =
      if (x == 0) None else Some(acc + x)

    val n = 100000L
    val expected = n*(n+1)/2
    val actual = (1L to n).toList.toNel.flatMap(_.reduceLeftM(Option.apply)(nonzero))
    actual should === (Some(expected))
  }

}

abstract class ReducibleCheck[F[_]: Reducible](name: String)(implicit ArbFInt: Arbitrary[F[Int]]) extends FoldableCheck[F](name) {
  def range(start: Long, endInclusive: Long): F[Long]

  test(s"Reducible[$name].reduceLeftM stack safety") {
    def nonzero(acc: Long, x: Long): Option[Long] =
      if (x == 0) None else Some(acc + x)

    val n = 100000L
    val expected = n*(n+1)/2
    val actual = range(1L, n).reduceLeftM(Option.apply)(nonzero)
    actual should === (Some(expected))
  }

  test(s"Reducible[$name].toNonEmptyList/toList consistency") {
    forAll { fa: F[Int] =>
      fa.toList.toNel should === (Some(fa.toNonEmptyList))
    }
  }
}
