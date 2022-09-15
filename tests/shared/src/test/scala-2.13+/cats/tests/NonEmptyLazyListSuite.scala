/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.tests

import cats._
import cats.data.NonEmptyLazyList
import cats.data.NonEmptyLazyListOps
import cats.data.NonEmptyVector
import cats.kernel.laws.discipline.EqTests
import cats.kernel.laws.discipline.HashTests
import cats.kernel.laws.discipline.OrderTests
import cats.kernel.laws.discipline.PartialOrderTests
import cats.kernel.laws.discipline.SemigroupTests
import cats.kernel.laws.discipline.SerializableTests
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.foldable._
import org.scalacheck.Prop._

class NonEmptyLazyListSuite extends NonEmptyCollectionSuite[LazyList, NonEmptyLazyList, NonEmptyLazyListOps] {
  protected def toList[A](value: NonEmptyLazyList[A]): List[A] = value.toList
  protected def underlyingToList[A](underlying: LazyList[A]): List[A] = underlying.toList
  protected def toNonEmptyCollection[A](nea: NonEmptyLazyList[A]): NonEmptyLazyListOps[A] = nea

  checkAll("NonEmptyLazyList[Int]", SemigroupTests[NonEmptyLazyList[Int]].semigroup)
  checkAll(s"Semigroup[NonEmptyLazyList]", SerializableTests.serializable(Semigroup[NonEmptyLazyList[Int]]))

  checkAll(s"NonEmptyLazyList[Int]", HashTests[NonEmptyLazyList[Int]].hash)
  checkAll(s"Hash[NonEmptyLazyList[Int]]", SerializableTests.serializable(Hash[NonEmptyLazyList[Int]]))

  checkAll("NonEmptyLazyList[Int]", NonEmptyAlternativeTests[NonEmptyLazyList].nonEmptyAlternative[Int, Int, Int])
  checkAll("NonEmptyAlternative[NonEmptyLazyList]",
           SerializableTests.serializable(NonEmptyAlternative[NonEmptyLazyList])
  )

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
  checkAll("NonEmptyLazyList[Int]", ShortCircuitingTests[NonEmptyLazyList].nonEmptyTraverse[Int])

  test("show") {
    assert(Show[NonEmptyLazyList[Int]].show(NonEmptyLazyList(1, 2, 3)) === "NonEmptyLazyList(1, ?)")
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
      assert(ci.size === (ci.toLazyList.size.toLong))
    }
  }

  test("filterNot and then exists should always be false") {
    forAll { (ci: NonEmptyLazyList[Int], f: Int => Boolean) =>
      assert(ci.filterNot(f).exists(f) === false)
    }
  }

  test("filter and then forall should always be true") {
    forAll { (ci: NonEmptyLazyList[Int], f: Int => Boolean) =>
      assert(ci.filter(f).forall(f) === true)
    }
  }

  test("exists should be consistent with find + isDefined") {
    forAll { (ci: NonEmptyLazyList[Int], f: Int => Boolean) =>
      assert(ci.exists(f) === (ci.find(f).isDefined))
    }
  }

  test("filterNot element and then contains should be false") {
    forAll { (ci: NonEmptyLazyList[Int], i: Int) =>
      assert(ci.filterNot(_ === i).contains(i) === false)
    }
  }

  test("fromNonEmptyVector . toNonEmptyVector is id") {
    forAll { (ci: NonEmptyLazyList[Int]) =>
      assert(NonEmptyLazyList.fromNonEmptyVector(ci.toNonEmptyVector) === ci)
    }
  }

  test("fromNonEmptyList . toNonEmptyList is id") {
    forAll { (ci: NonEmptyLazyList[Int]) =>
      assert(NonEmptyLazyList.fromNonEmptyList(ci.toNonEmptyList) === ci)
    }
  }

  test("fromLazyList . toLazyList is Option.some") {
    forAll { (ci: NonEmptyLazyList[Int]) =>
      assert(NonEmptyLazyList.fromLazyList(ci.toLazyList) === (Some(ci)))
    }
  }

  test("fromLazyListUnsafe throws exception when used with empty LazyList") {
    assert(Either.catchNonFatal(NonEmptyLazyList.fromLazyListUnsafe(LazyList.empty[Int])).isLeft === true)
  }

  test("fromLazyListAppend is consistent with LazyList#:+") {
    forAll { (lli: LazyList[Int], i: Int) =>
      assert(NonEmptyLazyList.fromLazyListAppend(lli, i).toLazyList === (lli :+ i))
    }
  }

  test("fromSeq . toList . iterator is id") {
    forAll { (ci: NonEmptyLazyList[Int]) =>
      assert(NonEmptyLazyList.fromSeq(ci.iterator.toList) === (Option(ci)))
    }
  }

  test("zipWith consistent with List#zip and then List#map") {
    forAll { (a: NonEmptyLazyList[String], b: NonEmptyLazyList[Int], f: (String, Int) => Int) =>
      assert(a.zipWith(b)(f).toList === a.toList.zip(b.toList).map { case (x, y) => f(x, y) })
    }
  }

  test("reverse . reverse is id") {
    forAll { (ci: NonEmptyLazyList[Int]) =>
      assert(ci.reverse.reverse === ci)
    }
  }

  test("reverse consistent with LazyList#reverse") {
    forAll { (ci: NonEmptyLazyList[Int]) =>
      assert(ci.reverse.toLazyList === (ci.toLazyList.reverse))
    }
  }

  test("NonEmptyLazyList#distinct is consistent with List#distinct") {
    forAll { (ci: NonEmptyLazyList[Int]) =>
      assert(ci.distinct.toList === (ci.toList.distinct))
    }
  }

  test("NonEmptyLazyList#toNev is consistent with List#toVector and creating NonEmptyVector from it") {
    forAll { (ci: NonEmptyLazyList[Int]) =>
      assert(ci.toNev === (NonEmptyVector.fromVectorUnsafe(Vector.empty[Int] ++ ci.toList.toVector)))
    }
  }

  test("Avoid all evaluation of NonEmptyLazyList#reduceRightTo") {
    val sum = implicitly[Reducible[NonEmptyLazyList]]
      .reduceRightTo(
        NonEmptyLazyList
          .fromLazyListPrepend(1, LazyList.from(2))
      )(identity) { (elem, acc) =>
        if (elem <= 100) acc.map(_ + elem) else Eval.later(0)
      }
      .value

    (1 to 100).sum === sum
  }
}

class ReducibleNonEmptyLazyListSuite extends ReducibleSuite[NonEmptyLazyList]("NonEmptyLazyList") {
  def iterator[T](nel: NonEmptyLazyList[T]): Iterator[T] = nel.toLazyList.iterator

  def range(start: Long, endInclusive: Long): NonEmptyLazyList[Long] =
    NonEmptyLazyList(start, (start + 1L).to(endInclusive): _*)

  def fromValues[A](el: A, els: A*): NonEmptyLazyList[A] = NonEmptyLazyList(el, els: _*)
}
