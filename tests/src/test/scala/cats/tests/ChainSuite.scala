package cats
package tests

import cats.data.Chain
import cats.kernel.laws.discipline.{MonoidTests, OrderTests}
import cats.laws.discipline.{AlternativeTests, CoflatMapTests, MonadTests, SerializableTests, TraverseTests}
import cats.laws.discipline.arbitrary._

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

  test("show"){
    Show[Chain[Int]].show(Chain(1, 2, 3)) should === ("Chain(1, 2, 3)")
    Chain.empty[Int].show should === ("Chain()")
    forAll { l: Chain[String] =>
      l.show should === (l.toString)
    }
  }

  test("size is consistent with toList.size") {
    forAll { (ci: Chain[Int]) =>
      ci.size.toInt should === (ci.toList.size)
    }
  }

  test("filterNot and then exists should always be false") {
    forAll { (ci: Chain[Int], f: Int => Boolean) =>
      ci.filterNot(f).exists(f) should === (false)
    }
  }

  test("filter and then forall should always be true") {
    forAll { (ci: Chain[Int], f: Int => Boolean) =>
      ci.filter(f).forall(f) should === (true)
    }
  }

  test("exists should be consistent with find + isDefined") {
    forAll { (ci: Chain[Int], f: Int => Boolean) =>
      ci.exists(f) should === (ci.find(f).isDefined)
    }
  }

  test("deleteFirst consistent with find") {
    forAll { (ci: Chain[Int], f: Int => Boolean) =>
      ci.find(f) should === (ci.deleteFirst(f).map(_._1))
    }
  }

  test("filterNot element and then contains should be false") {
    forAll { (ci: Chain[Int], i: Int) =>
      ci.filterNot(_ === i).contains(i) should === (false)
    }
  }

  test("Always nonempty after cons") {
    forAll { (ci: Chain[Int], i: Int) =>
      (i +: ci).nonEmpty should === (true)
    }
  }

  test("fromSeq . toVector is id") {
    forAll { (ci: Chain[Int]) =>
      Chain.fromSeq(ci.toVector) should === (ci)
    }
  }

  test("fromSeq . toList . iterator is id") {
    forAll { (ci: Chain[Int]) =>
      Chain.fromSeq(ci.iterator.toList) should === (ci)
    }
  }

  test("zipWith consistent with List#zip and then List#map") {
    forAll { (a: Chain[String], b: Chain[Int], f: (String, Int) => Int) =>
      a.zipWith(b)(f).toList should === (a.toList.zip(b.toList).map { case (x, y) => f(x, y) })
    }
  }

  test("groupBy consistent with List#groupBy") {
    forAll { (cs: Chain[String], f: String => Int) =>
      cs.groupBy(f).map { case (k, v) => (k, v.toList) }.toMap should === (cs.toList.groupBy(f).toMap)
    }
  }

  test("reverse . reverse is id") {
    forAll { (ci: Chain[Int]) =>
      ci.reverse.reverse should === (ci)
    }
  }

  test("reverse consistent with List#reverse") {
    forAll { (ci: Chain[Int]) =>
      ci.reverse.toList should === (ci.toList.reverse)
    }
  }
}
