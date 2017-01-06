package cats
package tests

import org.scalacheck.Arbitrary

import cats.data.NonEmptyList

class ReducibleTestsAdditional extends CatsSuite {

  test("Reducible[NonEmptyList].reduceLeftM stack safety") {
    def nonzero(acc: Long, x: Long): Option[Long] =
      if (x == 0) None else Some(acc + x)

    val n = 100000L
    val expected = n*(n+1)/2
    val actual = (1L to n).toList.toNel.flatMap(_.reduceLeftM(Option.apply)(nonzero))
    actual should === (Some(expected))
  }

  // exists method written in terms of reduceRightTo
  def contains[F[_]: Reducible, A: Eq](as: F[A], goal: A): Eval[Boolean] =
    as.reduceRightTo(_ === goal) { (a, lb) =>
      if (a === goal) Now(true) else lb
    }


  test("Reducible[NonEmptyList]") {
    val R = Reducible[NonEmptyList]

    // some basic sanity checks
    val tail = (2 to 10).toList
    val total = 1 + tail.sum
    val nel = NonEmptyList(1, tail)
    R.reduceLeft(nel)(_ + _) should === (total)
    R.reduceRight(nel)((x, ly) => ly.map(x + _)).value should === (total)
    R.reduce(nel) should === (total)

    // more basic checks
    val names = NonEmptyList.of("Aaron", "Betty", "Calvin", "Deirdra")
    val totalLength = names.toList.map(_.length).sum
    R.reduceLeftTo(names)(_.length)((sum, s) => s.length + sum) should === (totalLength)
    R.reduceMap(names)(_.length) should === (totalLength)
    val sumLeftM = R.reduceLeftM(names)(Some(_): Option[String]) { (acc, x) =>
      (Some(acc + x): Option[String])
    }
    assert(sumLeftM == Some("AaronBettyCalvinDeirdra"))
    val sumMapM = R.reduceMapM(names) { x => (Some(x): Option[String]) }
    assert(sumMapM == Some("AaronBettyCalvinDeirdra"))
    val isNotCalvin: String => Option[String] =
      x => if (x == "Calvin") None else Some(x)
    val notCalvin = R.reduceLeftM(names)(isNotCalvin) { (acc, x) =>
      isNotCalvin(x).map(acc + _)
    }
    assert(notCalvin == None)
    val notCalvinMapM = R.reduceMapM(names)(isNotCalvin)
    assert(notCalvinMapM == None)

    // test trampolining
    val large = NonEmptyList(1, (2 to 10000).toList)
    assert(contains(large, 10000).value)
  }

}

abstract class ReducibleCheck[F[_]: Reducible](name: String)(implicit ArbFInt: Arbitrary[F[Int]], ArbFString: Arbitrary[F[String]]) extends FoldableCheck[F](name) {
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

  test(s"Reducible[$name].intercalate1") {
    forAll { (fa: F[String], a: String) =>
      fa.intercalate1(a) === (fa.toList.mkString(a))
    }
  }
}
