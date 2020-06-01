package cats.tests

import cats.{Align, Bimonad, SemigroupK, Show, Traverse}
import cats.data.{NonEmptyLazyList, NonEmptyLazyListOps, NonEmptyVector}
import cats.kernel.{Eq, Hash, Order, PartialOrder, Semigroup}
import cats.kernel.laws.discipline.{EqTests, HashTests, OrderTests, PartialOrderTests, SemigroupTests}
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

class NonEmptyLazyListSuite extends NonEmptyCollectionSuite[LazyList, NonEmptyLazyList, NonEmptyLazyListOps] {
  protected def toList[A](value: NonEmptyLazyList[A]): List[A] = value.toList
  protected def underlyingToList[A](underlying: LazyList[A]): List[A] = underlying.toList
  protected def toNonEmptyCollection[A](nea: NonEmptyLazyList[A]): NonEmptyLazyListOps[A] = nea

  checkAll("NonEmptyLazyList[Int]", SemigroupTests[NonEmptyLazyList[Int]].semigroup)
  checkAll(s"Semigroup[NonEmptyLazyList]", SerializableTests.serializable(Semigroup[NonEmptyLazyList[Int]]))

  checkAll(s"NonEmptyLazyList[Int]", HashTests[NonEmptyLazyList[Int]].hash)
  checkAll(s"Hash[NonEmptyLazyList[Int]]", SerializableTests.serializable(Hash[NonEmptyLazyList[Int]]))

  checkAll("NonEmptyLazyList[Int]", SemigroupKTests[NonEmptyLazyList].semigroupK[Int])
  checkAll("SemigroupK[NonEmptyLazyList]", SerializableTests.serializable(SemigroupK[NonEmptyLazyList]))

  checkAll("NonEmptyLazyList[Int] with Option",
           NonEmptyTraverseTests[NonEmptyLazyList].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
  )
  checkAll("NonEmptyTraverse[NonEmptyLazyList]", SerializableTests.serializable(Traverse[NonEmptyLazyList]))

  checkAll("NonEmptyLazyList[Int]", BimonadTests[NonEmptyLazyList].bimonad[Int, Int, Int])
  checkAll("Bimonad[NonEmptyLazyList]", SerializableTests.serializable(Bimonad[NonEmptyLazyList]))

  checkAll("NonEmptyLazyList[Int]", OrderTests[NonEmptyLazyList[Int]].order)
  checkAll("Order[NonEmptyLazyList[Int]", SerializableTests.serializable(Order[NonEmptyLazyList[Int]]))

  checkAll("NonEmptyLazyList[Int]", AlignTests[NonEmptyLazyList].align[Int, Int, Int, Int])
  checkAll("Align[NonEmptyLazyList]", SerializableTests.serializable(Align[NonEmptyLazyList]))

  checkAll("NonEmptyLazyList[Int]", ShortCircuitingTests[NonEmptyLazyList].foldable[Int])
  checkAll("NonEmptyLazyList[Int]", ShortCircuitingTests[NonEmptyLazyList].traverse[Int])

  test("show") {
    Show[NonEmptyLazyList[Int]].show(NonEmptyLazyList(1, 2, 3)) should ===("NonEmptyLazyList(1, ?)")
  }
  checkAll("Show[NonEmptyLazyList[Int]]", SerializableTests.serializable(Show[NonEmptyLazyList[Int]]))

  {
    implicit val partialOrder: PartialOrder[ListWrapper[Int]] = ListWrapper.partialOrder[Int]
    checkAll("NonEmptyLazyList[ListWrapper[Int]]", PartialOrderTests[NonEmptyLazyList[ListWrapper[Int]]].partialOrder)
    checkAll("PartialOrder[NonEmptyLazyList[ListWrapper[Int]]",
             SerializableTests.serializable(PartialOrder[NonEmptyLazyList[ListWrapper[Int]]])
    )
  }

  {
    implicit val eqv: Eq[ListWrapper[Int]] = ListWrapper.eqv[Int]
    checkAll("NonEmptyLazyList[ListWrapper[Int]]", EqTests[NonEmptyLazyList[ListWrapper[Int]]].eqv)
    checkAll("Eq[NonEmptyLazyList[ListWrapper[Int]]",
             SerializableTests.serializable(Eq[NonEmptyLazyList[ListWrapper[Int]]])
    )
  }

  test("size is consistent with toLazyList.size") {
    forAll { (ci: NonEmptyLazyList[Int]) =>
      ci.size should ===(ci.toLazyList.size.toLong)
    }
  }

  test("filterNot and then exists should always be false") {
    forAll { (ci: NonEmptyLazyList[Int], f: Int => Boolean) =>
      ci.filterNot(f).exists(f) should ===(false)
    }
  }

  test("filter and then forall should always be true") {
    forAll { (ci: NonEmptyLazyList[Int], f: Int => Boolean) =>
      ci.filter(f).forall(f) should ===(true)
    }
  }

  test("exists should be consistent with find + isDefined") {
    forAll { (ci: NonEmptyLazyList[Int], f: Int => Boolean) =>
      ci.exists(f) should ===(ci.find(f).isDefined)
    }
  }

  test("filterNot element and then contains should be false") {
    forAll { (ci: NonEmptyLazyList[Int], i: Int) =>
      ci.filterNot(_ === i).contains(i) should ===(false)
    }
  }

  test("fromNonEmptyVector . toNonEmptyVector is id") {
    forAll { (ci: NonEmptyLazyList[Int]) =>
      NonEmptyLazyList.fromNonEmptyVector(ci.toNonEmptyVector) should ===(ci)
    }
  }

  test("fromNonEmptyList . toNonEmptyList is id") {
    forAll { (ci: NonEmptyLazyList[Int]) =>
      NonEmptyLazyList.fromNonEmptyList(ci.toNonEmptyList) should ===(ci)
    }
  }

  test("fromLazyList . toLazyList is Option.some") {
    forAll { (ci: NonEmptyLazyList[Int]) =>
      NonEmptyLazyList.fromLazyList(ci.toLazyList) should ===(Some(ci))
    }
  }

  test("fromLazyListUnsafe throws exception when used with empty LazyList") {
    Either.catchNonFatal(NonEmptyLazyList.fromLazyListUnsafe(LazyList.empty[Int])).isLeft should ===(true)
  }

  test("fromLazyListAppend is consistent with LazyList#:+") {
    forAll { (lli: LazyList[Int], i: Int) =>
      NonEmptyLazyList.fromLazyListAppend(lli, i).toLazyList should ===(lli :+ i)
    }
  }

  test("fromSeq . toList . iterator is id") {
    forAll { (ci: NonEmptyLazyList[Int]) =>
      NonEmptyLazyList.fromSeq(ci.iterator.toList) should ===(Option(ci))
    }
  }

  test("zipWith consistent with List#zip and then List#map") {
    forAll { (a: NonEmptyLazyList[String], b: NonEmptyLazyList[Int], f: (String, Int) => Int) =>
      a.zipWith(b)(f).toList should ===(a.toList.zip(b.toList).map { case (x, y) => f(x, y) })
    }
  }

  test("reverse . reverse is id") {
    forAll { (ci: NonEmptyLazyList[Int]) =>
      ci.reverse.reverse should ===(ci)
    }
  }

  test("reverse consistent with LazyList#reverse") {
    forAll { (ci: NonEmptyLazyList[Int]) =>
      ci.reverse.toLazyList should ===(ci.toLazyList.reverse)
    }
  }

  test("NonEmptyLazyList#distinct is consistent with List#distinct") {
    forAll { (ci: NonEmptyLazyList[Int]) =>
      ci.distinct.toList should ===(ci.toList.distinct)
    }
  }

  test("NonEmptyLazyList#toNev is consistent with List#toVector and creating NonEmptyVector from it") {
    forAll { (ci: NonEmptyLazyList[Int]) =>
      ci.toNev should ===(NonEmptyVector.fromVectorUnsafe(Vector.empty[Int] ++ ci.toList.toVector))
    }
  }
}

class ReducibleNonEmptyLazyListSuite extends ReducibleSuite[NonEmptyLazyList]("NonEmptyLazyList") {
  def iterator[T](nel: NonEmptyLazyList[T]): Iterator[T] = nel.toLazyList.iterator

  def range(start: Long, endInclusive: Long): NonEmptyLazyList[Long] =
    NonEmptyLazyList(start, (start + 1L).to(endInclusive): _*)

  def fromValues[A](el: A, els: A*): NonEmptyLazyList[A] = NonEmptyLazyList(el, els: _*)
}
