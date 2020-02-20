package cats.tests

import cats.{Align, Alternative, CoflatMap, Monad, Show, Traverse, TraverseFilter}
import cats.data.Chain
import cats.data.Chain.==:
import cats.data.Chain.`:==`
import cats.instances.all._
import cats.kernel.{Eq, Hash, Monoid, Order, PartialOrder}
import cats.kernel.laws.discipline.{EqTests, HashTests, MonoidTests, OrderTests, PartialOrderTests}
import cats.laws.discipline.{
  AlignTests,
  AlternativeTests,
  CoflatMapTests,
  MonadTests,
  SerializableTests,
  TraverseFilterTests,
  TraverseTests
}
import cats.laws.discipline.arbitrary._
import cats.syntax.foldable._
import cats.syntax.semigroup._

class ChainSuite extends CatsSuite {
  checkAll("Chain[Int]", AlternativeTests[Chain].alternative[Int, Int, Int])
  checkAll("Alternative[Chain]", SerializableTests.serializable(Alternative[Chain]))

  checkAll("Chain[Int] with Option", TraverseTests[Chain].traverse[Int, Int, Int, Set[Int], Option, Option])
  checkAll("Traverse[Chain]", SerializableTests.serializable(Traverse[Chain]))

  checkAll("Chain[Int]", MonadTests[Chain].monad[Int, Int, Int])
  checkAll("Monad[Chain]", SerializableTests.serializable(Monad[Chain]))

  checkAll("Chain[Int]", CoflatMapTests[Chain].coflatMap[Int, Int, Int])
  checkAll("Coflatmap[Chain]", SerializableTests.serializable(CoflatMap[Chain]))

  checkAll("Chain[Int]", MonoidTests[Chain[Int]].monoid)
  checkAll("Monoid[Chain]", SerializableTests.serializable(Monoid[Chain[Int]]))

  checkAll("Chain[Int]", OrderTests[Chain[Int]].order)
  checkAll("Order[Chain]", SerializableTests.serializable(Order[Chain[Int]]))

  checkAll("Chain[Int]", AlignTests[Chain].align[Int, Int, Int, Int])
  checkAll("Align[Chain]", SerializableTests.serializable(Align[Chain]))

  checkAll("Chain[Int]", TraverseFilterTests[Chain].traverseFilter[Int, Int, Int])
  checkAll("TraverseFilter[Chain]", SerializableTests.serializable(TraverseFilter[Chain]))

  {
    implicit val partialOrder: PartialOrder[ListWrapper[Int]] = ListWrapper.partialOrder[Int]
    checkAll("Chain[ListWrapper[Int]]", PartialOrderTests[Chain[ListWrapper[Int]]].partialOrder)
    checkAll("PartialOrder[Chain[ListWrapper[Int]]",
             SerializableTests.serializable(PartialOrder[Chain[ListWrapper[Int]]]))
  }

  {
    implicit val eqv: Eq[ListWrapper[Int]] = ListWrapper.eqv[Int]
    checkAll("Chain[ListWrapper[Int]]", EqTests[Chain[ListWrapper[Int]]].eqv)
    checkAll("Eq[Chain[ListWrapper[Int]]", SerializableTests.serializable(Eq[Chain[ListWrapper[Int]]]))
  }

  {
    implicit val hash: Hash[ListWrapper[Int]] = ListWrapper.hash[Int]
    checkAll("Chain[ListWrapper[Int]]", HashTests[Chain[ListWrapper[Int]]].hash)
    checkAll("Hash[Chain[ListWrapper[Int]]", SerializableTests.serializable(Hash[Chain[ListWrapper[Int]]]))
  }

  test("show") {
    Show[Chain[Int]].show(Chain(1, 2, 3)) should ===("Chain(1, 2, 3)")
    Chain.empty[Int].show should ===("Chain()")
    forAll { (l: Chain[String]) =>
      l.show should ===(l.toString)
    }
  }

  test("headOption") {
    forAll { (s: Seq[Int]) =>
      Chain.fromSeq(s).headOption should ===(s.headOption)
    }
  }

  test("lastOption") {
    forAll { (c: Chain[Int]) =>
      c.lastOption should ===(c.toList.lastOption)
    }
  }

  test("seq-like pattern match") {
    Chain(1, 2, 3) match {
      case Chain(a, b, c) => (a, b, c) should ===((1, 2, 3))
    }

    Chain(1, 2, 3) match {
      case h ==: t => (h, t) should ===(1 -> Chain(2, 3))
    }

    Chain(1, 2, 3) match {
      case init :== last => (init, last) should ===(Chain(1, 2) -> 3)
    }

  }

  test("size is consistent with toList.size") {
    forAll { (ci: Chain[Int]) =>
      ci.size.toInt should ===(ci.toList.size)
    }
  }

  test("filterNot and then exists should always be false") {
    forAll { (ci: Chain[Int], f: Int => Boolean) =>
      ci.filterNot(f).exists(f) should ===(false)
    }
  }

  test("filter and then forall should always be true") {
    forAll { (ci: Chain[Int], f: Int => Boolean) =>
      ci.filter(f).forall(f) should ===(true)
    }
  }

  test("exists should be consistent with find + isDefined") {
    forAll { (ci: Chain[Int], f: Int => Boolean) =>
      ci.exists(f) should ===(ci.find(f).isDefined)
    }
  }

  test("deleteFirst consistent with find") {
    forAll { (ci: Chain[Int], f: Int => Boolean) =>
      ci.find(f) should ===(ci.deleteFirst(f).map(_._1))
    }
  }

  test("filterNot element and then contains should be false") {
    forAll { (ci: Chain[Int], i: Int) =>
      ci.filterNot(_ === i).contains(i) should ===(false)
    }
  }

  test("Always nonempty after cons") {
    forAll { (ci: Chain[Int], i: Int) =>
      (i +: ci).nonEmpty should ===(true)
    }
  }

  test("fromSeq . toVector is id") {
    forAll { (ci: Chain[Int]) =>
      Chain.fromSeq(ci.toVector) should ===(ci)
    }
  }

  test("fromSeq . toList . iterator is id") {
    forAll { (ci: Chain[Int]) =>
      Chain.fromSeq(ci.iterator.toList) should ===(ci)
    }
  }

  test("zipWith consistent with List#zip and then List#map") {
    forAll { (a: Chain[String], b: Chain[Int], f: (String, Int) => Int) =>
      a.zipWith(b)(f).toList should ===(a.toList.zip(b.toList).map { case (x, y) => f(x, y) })
    }
  }

  test("groupBy consistent with List#groupBy") {
    forAll { (cs: Chain[String], f: String => Int) =>
      cs.groupBy(f).map { case (k, v) => (k, v.toList) }.toMap should ===(cs.toList.groupBy(f).toMap)
    }
  }

  test("zipWithIndex is consistent with toList.zipWithIndex") {
    forAll { (ci: Chain[Int]) =>
      ci.zipWithIndex.toList should ===(ci.toList.zipWithIndex)
    }
  }

  test("sortBy is consistent with toList.sortBy") {
    forAll { (ci: Chain[Int], f: Int => String) =>
      ci.sortBy(f).toList should ===(ci.toList.sortBy(f))
    }
  }

  test("sorted is consistent with toList.sorted") {
    forAll { (ci: Chain[Int]) =>
      ci.sorted.toList should ===(ci.toList.sorted)
    }
  }

  test("reverse . reverse is id") {
    forAll { (ci: Chain[Int]) =>
      ci.reverse.reverse should ===(ci)
    }
  }

  test("reverse consistent with List#reverse") {
    forAll { (ci: Chain[Int]) =>
      ci.reverse.toList should ===(ci.toList.reverse)
    }
  }

  test("(a ++ b).isEmpty ==> a.isEmpty and b.isEmpty") {
    forAll { (a: Chain[Int], b: Chain[Int]) =>
      assert((a ++ b).nonEmpty || (a.isEmpty && b.isEmpty))
    }
  }

  test("a.isEmpty == (a eq Chain.nil)") {
    assert(Chain.fromSeq(Nil) eq Chain.nil)

    forAll { (a: Chain[Int]) =>
      assert(a.isEmpty == (a eq Chain.nil))
    }
  }

  test("(nil ++ a) eq a") {
    forAll { (a: Chain[Int]) =>
      assert((Chain.nil ++ a) eq a)
      assert((a ++ Chain.nil) eq a)
    }
  }

  test("Chain.iterator.next should throw NoSuchElementException") {
    forAll { (a: Chain[Int]) =>
      val it = a.iterator

      while (it.hasNext) it.next

      intercept[java.util.NoSuchElementException] {
        it.next
      }

      val rit = a.reverseIterator

      while (rit.hasNext) rit.next

      intercept[java.util.NoSuchElementException] {
        rit.next
      }
    }
  }

  test("Chain#distinct is consistent with List#distinct") {
    forAll { (a: Chain[Int]) =>
      a.distinct.toList should ===(a.toList.distinct)
    }
  }

  test("=== is consistent with == (issue #2540)") {
    (Chain.one(1) |+| Chain.one(2) |+| Chain.one(3)) should be(Chain.fromSeq(List(1, 2, 3)))

    forAll { (a: Chain[Int], b: Chain[Int]) =>
      (a === b) should ===(a == b)
    }
  }

  test("== returns false for non-Chains") {
    forAll { (a: Chain[Int], b: Int) =>
      (a == b) should ===(false)
    }
  }

  test("== returns false for Chains of different element types") {
    forAll { (a: Chain[Option[String]], b: Chain[String]) =>
      (a == b) should ===(a.isEmpty && b.isEmpty)
    }
  }

  test("Chain#hashCode is consistent with List#hashCode") {
    forAll { (x: Chain[Int]) =>
      x.hashCode should ===(x.toList.hashCode)
    }
  }

  test("Chain#takeWhile is consistent with List#takeWhile") {
    forAll { (x: Chain[Int], p: Int => Boolean) =>
      x.takeWhile(p).toList should ===(x.toList.takeWhile(p))
    }
  }

  test("Chain#dropWhile is consistent with List#dropWhile") {
    forAll { (x: Chain[Int], p: Int => Boolean) =>
      x.dropWhile(p).toList should ===(x.toList.dropWhile(p))
    }
  }

  test("Chain#get is consistent with List#lift") {
    forAll { (x: Chain[Int], idx: Int) =>
      x.get(idx.toLong) should ===(x.toList.lift(idx))
    }
  }

}
