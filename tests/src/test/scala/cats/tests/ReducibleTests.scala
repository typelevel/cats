package cats
package tests

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


