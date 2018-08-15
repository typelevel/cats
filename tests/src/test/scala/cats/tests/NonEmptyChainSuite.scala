package cats
package tests

import cats.data.NonEmptyChain
import cats.kernel.laws.discipline.SemigroupTests
import cats.laws.discipline.{BimonadTests, NonEmptyTraverseTests, SemigroupKTests, SerializableTests}
import cats.laws.discipline.arbitrary._

class NonEmptyChainSuite extends CatsSuite {
  checkAll("NonEmptyChain[Int]", SemigroupKTests[NonEmptyChain].semigroupK[Int])
  checkAll("SemigroupK[NonEmptyChain]", SerializableTests.serializable(SemigroupK[NonEmptyChain]))

  checkAll("NonEmptyChain[Int] with Option", NonEmptyTraverseTests[NonEmptyChain].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option])
  checkAll("NonEmptyTraverse[NonEmptyChain]", SerializableTests.serializable(Traverse[NonEmptyChain]))

  checkAll("NonEmptyChain[Int]", BimonadTests[NonEmptyChain].bimonad[Int, Int, Int])
  checkAll("Bimonad[NonEmptyChain]", SerializableTests.serializable(Bimonad[NonEmptyChain]))

  checkAll("NonEmptyChain[Int]", SemigroupTests[NonEmptyChain[Int]].semigroup)
  checkAll("Monoid[NonEmptyChain]", SerializableTests.serializable(Semigroup[NonEmptyChain[Int]]))

  test("show"){
    Show[NonEmptyChain[Int]].show(NonEmptyChain(1, 2, 3)) should === ("NonEmptyChain(1, 2, 3)")
  }

  test("size is consistent with toChain.size") {
    forAll { (ci: NonEmptyChain[Int]) =>
      ci.size should === (ci.toChain.size)
    }
  }

  test("filterNot and then exists should always be false") {
    forAll { (ci: NonEmptyChain[Int], f: Int => Boolean) =>
      ci.filterNot(f).exists(f) should === (false)
    }
  }

  test("filter and then forall should always be true") {
    forAll { (ci: NonEmptyChain[Int], f: Int => Boolean) =>
      ci.filter(f).forall(f) should === (true)
    }
  }

  test("exists should be consistent with find + isDefined") {
    forAll { (ci: NonEmptyChain[Int], f: Int => Boolean) =>
      ci.exists(f) should === (ci.find(f).isDefined)
    }
  }

  test("deleteFirst consistent with find") {
    forAll { (ci: NonEmptyChain[Int], f: Int => Boolean) =>
      ci.find(f) should === (ci.deleteFirst(f).map(_._1))
    }
  }

  test("filterNot element and then contains should be false") {
    forAll { (ci: NonEmptyChain[Int], i: Int) =>
      ci.filterNot(_ === i).contains(i) should === (false)
    }
  }

  test("Always nonempty after cons") {
    forAll { (ci: NonEmptyChain[Int], i: Int) =>
      (i +: ci).nonEmpty should === (true)
    }
  }

  test("fromNonEmptyVector . toNonEmptyVector is id") {
    forAll { (ci: NonEmptyChain[Int]) =>
      NonEmptyChain.fromNonEmptyVector(ci.toNonEmptyVector) should === (ci)
    }
  }

  test("fromSeq . toList . iterator is id") {
    forAll { (ci: NonEmptyChain[Int]) =>
      NonEmptyChain.fromSeq(ci.iterator.toList) should === (Option(ci))
    }
  }

  test("zipWith consistent with List#zip and then List#map") {
    forAll { (a: NonEmptyChain[String], b: NonEmptyChain[Int], f: (String, Int) => Int) =>
      a.zipWith(b)(f).toList should === (a.toList.zip(b.toList).map { case (x, y) => f(x, y) })
    }
  }

  test("groupBy consistent with List#groupBy") {
    forAll { (cs: NonEmptyChain[String], f: String => Int) =>
      cs.groupBy(f).map(_.toNonEmptyList) should === (cs.toNonEmptyList.groupByNem(f))
    }
  }

  test("reverse . reverse is id") {
    forAll { (ci: NonEmptyChain[Int]) =>
      ci.reverse.reverse should === (ci)
    }
  }

  test("reverse consistent with Chain#reverse") {
    forAll { (ci: NonEmptyChain[Int]) =>
      ci.reverse.toChain should === (ci.toChain.reverse)
    }
  }
}
