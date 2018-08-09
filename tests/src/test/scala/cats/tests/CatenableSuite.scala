package cats
package tests

import cats.data.Catenable
import cats.kernel.laws.discipline.MonoidTests
import cats.laws.discipline.{AlternativeTests, MonadTests, SerializableTests, TraverseTests}
import cats.laws.discipline.arbitrary._

class CatenableSuite extends CatsSuite {
  checkAll("Catenable[Int]", AlternativeTests[Catenable].alternative[Int, Int, Int])
  checkAll("Alternative[Catenable]", SerializableTests.serializable(Alternative[Catenable]))

  checkAll("Catenable[Int] with Option", TraverseTests[Catenable].traverse[Int, Int, Int, Set[Int], Option, Option])
  checkAll("Traverse[Catenable]", SerializableTests.serializable(Traverse[Catenable]))

  checkAll("Catenable[Int]", MonadTests[Catenable].monad[Int, Int, Int])
  checkAll("Monad[Catenable]", SerializableTests.serializable(Monad[Catenable]))

  checkAll("Catenable[Int]", MonoidTests[Catenable[Int]].monoid)
  checkAll("Monoid[Catenable]", SerializableTests.serializable(Monoid[Catenable[Int]]))

  test("show"){
    Show[Catenable[Int]].show(Catenable(1, 2, 3)) should === ("Catenable(1, 2, 3)")
    Catenable.empty[Int].show should === ("Catenable()")
    forAll { l: Catenable[String] =>
      l.show should === (l.toString)
    }
  }

  test("size is consistent with toList.size") {
    forAll { (ci: Catenable[Int]) =>
      ci.size should === (ci.toList.size)
    }
  }

  test("filterNot and then exists should always be false") {
    forAll { (ci: Catenable[Int], f: Int => Boolean) =>
      ci.filterNot(f).exists(f) should === (false)
    }
  }

  test("filter and then forall should always be true") {
    forAll { (ci: Catenable[Int], f: Int => Boolean) =>
      ci.filter(f).forall(f) should === (true)
    }
  }

  test("exists should be consistent with find + isDefined") {
    forAll { (ci: Catenable[Int], f: Int => Boolean) =>
      ci.exists(f) should === (ci.find(f).isDefined)
    }
  }

  test("deleteFirst consistent with find") {
    forAll { (ci: Catenable[Int], f: Int => Boolean) =>
      ci.find(f) should === (ci.deleteFirst(f).map(_._1))
    }
  }

  test("filterNot element and then contains should be false") {
    forAll { (ci: Catenable[Int], i: Int) =>
      ci.filterNot(_ === i).contains(i) should === (false)
    }
  }

  test("Always nonempty after cons") {
    forAll { (ci: Catenable[Int], i: Int) =>
      (i +: ci).nonEmpty should === (true)
    }
  }

  test("fromSeq . toVector is id") {
    forAll { (ci: Catenable[Int]) =>
      Catenable.fromSeq(ci.toVector) should === (ci)
    }
  }

  test("fromSeq . toList . iterator is id") {
    forAll { (ci: Catenable[Int]) =>
      Catenable.fromSeq(ci.iterator.toList) should === (ci)
    }
  }

  test("zipWith consistent with List#zip and then List#map") {
    forAll { (a: Catenable[String], b: Catenable[Int], f: (String, Int) => Int) =>
      a.zipWith(b)(f).toList should === (a.toList.zip(b.toList).map { case (x, y) => f(x, y) })
    }
  }

  test("groupBy consistent with List#groupBy") {
    forAll { (cs: Catenable[String], f: String => Int) =>
      cs.groupBy(f).map { case (k, v) => (k, v.toList) }.toMap should === (cs.toList.groupBy(f).toMap)
    }
  }

  test("reverse . reverse is id") {
    forAll { (ci: Catenable[Int]) =>
      ci.reverse.reverse should === (ci)
    }
  }

  test("reverse consistent with List#reverse") {
    forAll { (ci: Catenable[Int]) =>
      ci.reverse.toList should === (ci.toList.reverse)
    }
  }
}
