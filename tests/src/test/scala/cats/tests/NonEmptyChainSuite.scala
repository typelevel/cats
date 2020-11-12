package cats.tests

import cats.{Align, Bimonad, SemigroupK, Show, Traverse}
import cats.data.{Chain, NonEmptyChain, NonEmptyChainOps}
import cats.kernel.{Eq, Order, PartialOrder, Semigroup}
import cats.kernel.laws.discipline.{EqTests, OrderTests, PartialOrderTests, SemigroupTests}
import cats.laws.discipline.{
  AlignTests,
  BimonadTests,
  NonEmptyTraverseTests,
  SemigroupKTests,
  SerializableTests,
  ShortCircuitingTests
}
import cats.laws.discipline.arbitrary._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.eq._
import org.scalacheck.Prop._

class NonEmptyChainSuite extends NonEmptyCollectionSuite[Chain, NonEmptyChain, NonEmptyChainOps] {
  protected def toList[A](value: NonEmptyChain[A]): List[A] = value.toChain.toList
  protected def underlyingToList[A](underlying: Chain[A]): List[A] = underlying.toList
  protected def toNonEmptyCollection[A](nea: NonEmptyChain[A]): NonEmptyChainOps[A] = nea

  checkAll("NonEmptyChain[Int]", SemigroupKTests[NonEmptyChain].semigroupK[Int])
  checkAll("SemigroupK[NonEmptyChain]", SerializableTests.serializable(SemigroupK[NonEmptyChain]))

  checkAll("NonEmptyChain[Int] with Option",
           NonEmptyTraverseTests[NonEmptyChain].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
  )
  checkAll("NonEmptyTraverse[NonEmptyChain]", SerializableTests.serializable(Traverse[NonEmptyChain]))

  checkAll("NonEmptyChain[Int]", BimonadTests[NonEmptyChain].bimonad[Int, Int, Int])
  checkAll("Bimonad[NonEmptyChain]", SerializableTests.serializable(Bimonad[NonEmptyChain]))

  checkAll("NonEmptyChain[Int]", SemigroupTests[NonEmptyChain[Int]].semigroup)
  checkAll("Monoid[NonEmptyChain]", SerializableTests.serializable(Semigroup[NonEmptyChain[Int]]))

  checkAll("NonEmptyChain[Int]", OrderTests[NonEmptyChain[Int]].order)
  checkAll("Order[NonEmptyChain[Int]", SerializableTests.serializable(Order[NonEmptyChain[Int]]))

  checkAll("NonEmptyChain[Int]", AlignTests[NonEmptyChain].align[Int, Int, Int, Int])
  checkAll("Align[NonEmptyChain]", SerializableTests.serializable(Align[NonEmptyChain]))

  checkAll("NonEmptyChain[Int]", ShortCircuitingTests[NonEmptyChain].foldable[Int])
  checkAll("NonEmptyChain[Int]", ShortCircuitingTests[NonEmptyChain].traverse[Int])
  checkAll("NonEmptyChain[Int]", ShortCircuitingTests[NonEmptyChain].nonEmptyTraverse[Int])

  {
    implicit val partialOrder: PartialOrder[ListWrapper[Int]] = ListWrapper.partialOrder[Int]
    checkAll("NonEmptyChain[ListWrapper[Int]]", PartialOrderTests[NonEmptyChain[ListWrapper[Int]]].partialOrder)
    checkAll("PartialOrder[NonEmptyChain[ListWrapper[Int]]",
             SerializableTests.serializable(PartialOrder[NonEmptyChain[ListWrapper[Int]]])
    )
  }

  {
    implicit val eqv: Eq[ListWrapper[Int]] = ListWrapper.eqv[Int]
    checkAll("NonEmptyChain[ListWrapper[Int]]", EqTests[NonEmptyChain[ListWrapper[Int]]].eqv)
    checkAll("Eq[NonEmptyChain[ListWrapper[Int]]", SerializableTests.serializable(Eq[NonEmptyChain[ListWrapper[Int]]]))
  }

  test("show") {
    assert(Show[NonEmptyChain[Int]].show(NonEmptyChain(1, 2, 3)) === "NonEmptyChain(1, 2, 3)")
  }

  test("size is consistent with toChain.size") {
    forAll { (ci: NonEmptyChain[Int]) =>
      assert(ci.size === (ci.toChain.size))
    }
  }

  test("filterNot and then exists should always be false") {
    forAll { (ci: NonEmptyChain[Int], f: Int => Boolean) =>
      assert(ci.filterNot(f).exists(f) === false)
    }
  }

  test("filter and then forall should always be true") {
    forAll { (ci: NonEmptyChain[Int], f: Int => Boolean) =>
      assert(ci.filter(f).forall(f))
    }
  }

  test("exists should be consistent with find + isDefined") {
    forAll { (ci: NonEmptyChain[Int], f: Int => Boolean) =>
      assert(ci.exists(f) === (ci.find(f).isDefined))
    }
  }

  test("deleteFirst consistent with find") {
    forAll { (ci: NonEmptyChain[Int], f: Int => Boolean) =>
      assert(ci.find(f) === (ci.deleteFirst(f).map(_._1)))
    }
  }

  test("filterNot element and then contains should be false") {
    forAll { (ci: NonEmptyChain[Int], i: Int) =>
      assert(ci.filterNot(_ === i).contains(i) === false)
    }
  }

  test("Always nonempty after cons") {
    forAll { (ci: NonEmptyChain[Int], i: Int) =>
      assert((i +: ci).nonEmpty)
    }
  }

  test("fromNonEmptyVector . toNonEmptyVector is id") {
    forAll { (ci: NonEmptyChain[Int]) =>
      assert(NonEmptyChain.fromNonEmptyVector(ci.toNonEmptyVector) === ci)
    }
  }

  test("fromNonEmptyList . toNonEmptyList is id") {
    forAll { (ci: NonEmptyChain[Int]) =>
      assert(NonEmptyChain.fromNonEmptyList(ci.toNonEmptyList) === ci)
    }
  }

  test("fromChain . toChain is Option.some") {
    forAll { (ci: NonEmptyChain[Int]) =>
      assert(NonEmptyChain.fromChain(ci.toChain) === (Some(ci)))
    }
  }

  test("fromChainUnsafe throws exception when used with empty chain") {
    assert(Either.catchNonFatal(NonEmptyChain.fromChainUnsafe(Chain.empty[Int])).isLeft === true)
  }

  test("fromSeq . toList . iterator is id") {
    forAll { (ci: NonEmptyChain[Int]) =>
      assert(NonEmptyChain.fromSeq(ci.iterator.toSeq) === (Option(ci)))
    }
  }

  test("zipWith consistent with List#zip and then List#map") {
    forAll { (a: NonEmptyChain[String], b: NonEmptyChain[Int], f: (String, Int) => Int) =>
      assert(a.zipWith(b)(f).toList === (a.toList.zip(b.toList).map { case (x, y) => f(x, y) }))
    }
  }

  test("groupBy consistent with NonEmptyList#groupByNem") {
    forAll { (cs: NonEmptyChain[String], key: String => Int) =>
      assert(cs.groupBy(key).map(_.toNonEmptyList) === (cs.toNonEmptyList.groupByNem(key)))
    }
  }

  test("groupMap consistent with NonEmptyList#groupMapNem") {
    forAll { (cs: NonEmptyChain[String], key: String => String, f: String => Int) =>
      assert(cs.groupMap(key)(f).map(_.toNonEmptyList) === (cs.toNonEmptyList.groupMapNem(key)(f)))
    }
  }

  test("groupMapReduce consistent with NonEmptyList#groupMapReduceNem") {
    forAll { (cs: NonEmptyChain[String], key: String => String, f: String => Int) =>
      assert(cs.groupMapReduce(key)(f) === (cs.toNonEmptyList.groupMapReduceNem(key)(f)))
    }
  }

  test("groupMapReduceWith consistent with NonEmptyList#groupMapReduceWithNem") {
    forAll { (cs: NonEmptyChain[String], key: String => String, f: String => Int, combine: (Int, Int) => Int) =>
      assert(cs.groupMapReduceWith(key)(f)(combine) === (cs.toNonEmptyList.groupMapReduceWithNem(key)(f)(combine)))
    }
  }

  test("reverse . reverse is id") {
    forAll { (ci: NonEmptyChain[Int]) =>
      assert(ci.reverse.reverse === ci)
    }
  }

  test("reverse consistent with Chain#reverse") {
    forAll { (ci: NonEmptyChain[Int]) =>
      assert(ci.reverse.toChain === (ci.toChain.reverse))
    }
  }

  test("NonEmptyChain#distinct is consistent with List#distinct") {
    forAll { (ci: NonEmptyChain[Int]) =>
      assert(ci.distinct.toList === (ci.toList.distinct))
    }
  }

  test("init") {
    forAll { (ci: NonEmptyChain[Int]) =>
      assert(ci.init.toList === (ci.toList.init))
    }
  }

  test("last") {
    forAll { (ci: NonEmptyChain[Int]) =>
      assert(ci.last === (ci.toList.last))
    }
  }
}

class ReducibleNonEmptyChainSuite extends ReducibleSuite[NonEmptyChain]("NonEmptyChain") {
  def iterator[T](nel: NonEmptyChain[T]): Iterator[T] = nel.toChain.iterator

  def range(start: Long, endInclusive: Long): NonEmptyChain[Long] =
    NonEmptyChain(start, (start + 1L).to(endInclusive): _*)

  def fromValues[A](el: A, els: A*): NonEmptyChain[A] = NonEmptyChain(el, els: _*)
}
