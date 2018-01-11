package cats
package tests

import org.scalacheck.Arbitrary
import cats.data.NonEmptyList

class ReducibleSuiteAdditional extends CatsSuite {

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

  test("Reducible[NonEmptyList] default get/size implementation") {
    val R = new NonEmptyReducible[NonEmptyList, List] {
      def split[A](nel: NonEmptyList[A]): (A, List[A]) = (nel.head, nel.tail)
    }
    val nel = NonEmptyList.of(1, 2, 3)
    R.get(nel)(1L) should === (nel.get(1L))
    R.size(nel) should === (nel.size.toLong)
    R.get(nel)(4L) should === (None)
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

abstract class ReducibleSuite[F[_]: Reducible](name: String)(
  implicit ArbFInt: Arbitrary[F[Int]],
    ArbFString: Arbitrary[F[String]]) extends FoldableSuite[F](name) {

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

  test(s"Reducible[$name].nonEmptyIntercalate") {
    forAll { (fa: F[String], a: String) =>
      fa.nonEmptyIntercalate(a) === (fa.toList.mkString(a))
    }
  }


  test("Reducible#nonEmptyPartition retains size") {
    forAll { (fi: F[Int], f: Int => Either[String, String]) =>
      val folded = fi.nonEmptyPartition(f).fold(identity, identity, _ ++ _.toList)
      folded.size.toLong should === (fi.size)
    }
  }

  test("Reducible#nonEmptyPartition to one side is identity") {
    forAll { (fi: F[Int], f: Int => String) =>
      val g: Int => Either[Double, String] = f andThen Right.apply
      val h: Int => Either[String, Double] = f andThen Left.apply

      val withG = fi.nonEmptyPartition(g).right.getOrElse(NonEmptyList.one(""))
      withG should === (Reducible[F].toNonEmptyList(fi).map(f))

      val withH = fi.nonEmptyPartition(h).left.getOrElse(NonEmptyList.one(""))
      withH should === (Reducible[F].toNonEmptyList(fi).map(f))
    }
  }

}
