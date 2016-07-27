package cats
package tests

import org.scalatest.prop.PropertyChecks
import org.scalacheck.Arbitrary

import cats.instances.all._

abstract class FoldableCheck[F[_]: Foldable](name: String)(implicit ArbFInt: Arbitrary[F[Int]]) extends CatsSuite with PropertyChecks {

  def iterator[T](fa: F[T]): Iterator[T]

  test("size") {
    forAll { (fa: F[Int]) =>
      fa.size should === (iterator(fa).size.toLong)
    }
  }

  test("summation") {
    forAll { (fa: F[Int]) =>
      val total = iterator(fa).sum
      fa.foldLeft(0)(_ + _) should === (total)
      fa.foldRight(Now(0))((x, ly) => ly.map(x + _)).value should === (total)
      fa.fold should === (total)
      fa.foldMap(identity) should === (total)
    }
  }

  test("find/exists/forall/filter_/dropWhile_") {
    forAll { (fa: F[Int], n: Int) =>
      fa.find(_ > n)   should === (iterator(fa).find(_ > n))
      fa.exists(_ > n) should === (iterator(fa).exists(_ > n))
      fa.forall(_ > n) should === (iterator(fa).forall(_ > n))
      fa.filter_(_ > n) should === (iterator(fa).filter(_ > n).toList)
      fa.dropWhile_(_ > n) should === (iterator(fa).dropWhile(_ > n).toList)
      fa.takeWhile_(_ > n) should === (iterator(fa).takeWhile(_ > n).toList)
    }
  }

  test("toList/isEmpty/nonEmpty") {
    forAll { (fa: F[Int]) =>
      fa.toList should === (iterator(fa).toList)
      fa.isEmpty should === (iterator(fa).isEmpty)
      fa.nonEmpty should === (iterator(fa).nonEmpty)
    }
  }

  test("maximum/minimum") {
    forAll { (fa: F[Int]) =>
      val maxOpt = fa.maximumOption
      val minOpt = fa.minimumOption
      val list = fa.toList
      val nelOpt = list.toNel
      maxOpt should === (nelOpt.map(_.maximum))
      maxOpt should === (nelOpt.map(_.toList.max))
      minOpt should === (nelOpt.map(_.minimum))
      minOpt should === (nelOpt.map(_.toList.min))
      maxOpt.forall(i => fa.forall(_ <= i)) should === (true)
      minOpt.forall(i => fa.forall(_ >= i)) should === (true)
    }
  }

  test("reduceLeftOption/reduceRightOption") {
    forAll { (fa: F[Int]) =>
      val list = fa.toList
      fa.reduceLeftOption(_ - _) should === (list.reduceLeftOption(_ - _))
      fa.reduceRightOption((x, ly) => ly.map(x - _)).value should === (list.reduceRightOption(_ - _))
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
    F.foldLeft(ns, 0)(_ + _) should === (total)
    F.foldRight(ns, Now(0))((x, ly) => ly.map(x + _)).value should === (total)
    F.fold(ns) should === (total)

    // more basic checks
    val names = List("Aaron", "Betty", "Calvin", "Deirdra")
    F.foldMap(names)(_.length) should === (names.map(_.length).sum)
    val sumM = F.foldM(names, "") { (acc, x) => (Some(acc + x): Option[String]) }
    assert(sumM == Some("AaronBettyCalvinDeirdra"))
    val notCalvin = F.foldM(names, "") { (acc, x) =>
      if (x == "Calvin") (None: Option[String])
      else (Some(acc + x): Option[String]) }
    assert(notCalvin == None)

    // test trampolining
    val large = (1 to 10000).toList
    assert(contains(large, 10000).value)

    // safely build large lists
    val larger = F.foldRight(large, Now(List.empty[Int]))((x, lxs) => lxs.map((x + 1) :: _))
    larger.value should === (large.map(_ + 1))
  }

  test("Foldable[List].foldM stack safety") {
    def nonzero(acc: Long, x: Long): Option[Long] =
      if (x == 0) None else Some(acc + x)

    val n = 100000L
    val expected = n*(n+1)/2
    val actual = Foldable[List].foldM((1L to n).toList, 0L)(nonzero)
    assert(actual.get == expected)
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

    // test trampolining
    val large = Stream((1 to 10000): _*)
    assert(contains(large, 10000).value)
  }
}

class FoldableListCheck extends FoldableCheck[List]("list") {
  def iterator[T](list: List[T]): Iterator[T] = list.iterator
}

class FoldableVectorCheck extends FoldableCheck[Vector]("vector") {
  def iterator[T](vector: Vector[T]): Iterator[T] = vector.iterator
}

class FoldableSetCheck extends FoldableCheck[Set]("set") {
  def iterator[T](set: Set[T]): Iterator[T] = set.iterator
}

class FoldableStreamCheck extends FoldableCheck[Stream]("stream") {
  def iterator[T](stream: Stream[T]): Iterator[T] = stream.iterator
}

class FoldableMapCheck extends FoldableCheck[Map[Int, ?]]("map") {
  def iterator[T](map: Map[Int, T]): Iterator[T] = map.iterator.map(_._2)
}
