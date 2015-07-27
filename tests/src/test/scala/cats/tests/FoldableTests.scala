package cats
package tests

import cats.laws.discipline.ArbitraryK

import org.scalatest.prop.PropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary

abstract class FoldableCheck[F[_]: ArbitraryK: Foldable](name: String) extends CatsSuite with PropertyChecks {

  def iterator[T](fa: F[T]): Iterator[T]

  implicit val arbfn: Arbitrary[F[Int]] = ArbitraryK[F].synthesize[Int]

  test("summation") {
    forAll { (fa: F[Int]) =>
      val total = iterator(fa).sum
      fa.foldLeft(0)(_ + _) shouldBe total
      fa.foldRight(Now(0))((x, ly) => ly.map(x + _)).value shouldBe total
      fa.fold shouldBe total
      fa.foldMap(identity) shouldBe total
    }
  }

  test("find/exists/forall/filter_/dropWhile_") {
    forAll { (fa: F[Int], n: Int) =>
      fa.find(_ > n)   shouldBe iterator(fa).find(_ > n)
      fa.exists(_ > n) shouldBe iterator(fa).exists(_ > n)
      fa.forall(_ > n) shouldBe iterator(fa).forall(_ > n)
      fa.filter_(_ > n) shouldBe iterator(fa).filter(_ > n).toList
      fa.dropWhile_(_ > n) shouldBe iterator(fa).dropWhile(_ > n).toList
    }
  }

  test("toList/isEmpty/nonEmpty") {
    forAll { (fa: F[Int]) =>
      fa.toList shouldBe iterator(fa).toList
      fa.isEmpty shouldBe iterator(fa).isEmpty
      fa.nonEmpty shouldBe iterator(fa).nonEmpty
    }
  }
}

class FoldableTestsAdditional extends CatsSuite {

  // exists method written in terms of foldRight
  def contains[F[_]: Foldable, A: Eq](as: F[A], goal: A): Eval[Boolean] =
    as.foldRight(Now(false)) { (a, lb) =>
      if (a === goal) Now(true) else lb
    }


  test("Foldable[List]") {
    val F = Foldable[List]

    // some basic sanity checks
    val ns = (1 to 10).toList
    val total = ns.sum
    assert(F.foldLeft(ns, 0)(_ + _) == total)
    assert(F.foldRight(ns, Now(0))((x, ly) => ly.map(x + _)).value == total)
    assert(F.fold(ns) == total)

    // more basic checks
    val names = List("Aaron", "Betty", "Calvin", "Deirdra")
    assert(F.foldMap(names)(_.length) == names.map(_.length).sum)

    // test trampolining
    val large = (1 to 10000).toList
    assert(contains(large, 10000).value)

    // safely build large lists
    val larger = F.foldRight(large, Now(List.empty[Int]))((x, lxs) => lxs.map((x + 1) :: _))
    assert(larger.value == large.map(_ + 1))
  }

  test("Foldable[Stream]") {
    val F = Foldable[Stream]

    def bomb[A]: A = sys.error("boom")
    val dangerous = 0 #:: 1 #:: 2 #:: bomb[Stream[Int]]

    // doesn't blow up - this also ensures it works for infinite streams.
    assert(contains(dangerous, 2).value)

    // lazy results don't blow up unless you call .value on them.
    val doom: Eval[Boolean] = contains(dangerous, -1)

    // ensure that the Lazy[B] param to foldRight is actually being
    // handled lazily. it only needs to be evaluated if we reach the
    // "end" of the fold.
    val trap = Eval.later(bomb[Boolean])
    val result = F.foldRight(1 #:: 2 #:: Stream.empty, trap) { (n, lb) =>
      if (n == 2) Now(true) else lb
    }
    assert(result.value)
  }
}
