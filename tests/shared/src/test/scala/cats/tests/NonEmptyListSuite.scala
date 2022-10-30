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
import cats.data.NonEmptyList
import cats.data.NonEmptyList.ZipNonEmptyList
import cats.data.NonEmptyMap
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.kernel.laws.discipline.EqTests
import cats.kernel.laws.discipline.OrderTests
import cats.kernel.laws.discipline.PartialOrderTests
import cats.kernel.laws.discipline.SemigroupTests
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.syntax.eq._
import cats.syntax.foldable._
import cats.syntax.reducible._
import cats.syntax.show._
import org.scalacheck.Prop._
import org.scalacheck.Test.Parameters

import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

class NonEmptyListSuite extends NonEmptyCollectionSuite[List, NonEmptyList, NonEmptyList] {
  protected def toList[A](value: NonEmptyList[A]): List[A] = value.toList
  protected def underlyingToList[A](underlying: List[A]): List[A] = underlying
  protected def toNonEmptyCollection[A](nea: NonEmptyList[A]): NonEmptyList[A] = nea

  // Lots of collections here.. telling ScalaCheck to calm down a bit
  implicit override val scalaCheckTestParameters: Parameters =
    Parameters.default.withMinSuccessfulTests(20).withMaxSize(Parameters.default.minSize + 5)

  checkAll("NonEmptyList[Int]", OrderTests[NonEmptyList[Int]].order)

  checkAll("NonEmptyList[Int] with Option",
           NonEmptyTraverseTests[NonEmptyList].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
  )
  checkAll("NonEmptyTraverse[NonEmptyList[A]]", SerializableTests.serializable(NonEmptyTraverse[NonEmptyList]))

  checkAll("NonEmptyList[Int]", ReducibleTests[NonEmptyList].reducible[Option, Int, Int])
  checkAll("Reducible[NonEmptyList]", SerializableTests.serializable(Reducible[NonEmptyList]))

  checkAll("NonEmptyList[Int]", NonEmptyAlternativeTests[NonEmptyList].nonEmptyAlternative[Int, Int, Int])
  checkAll("NonEmptyAlternative[NonEmptyList[A]]", SerializableTests.serializable(NonEmptyAlternative[NonEmptyList]))

  checkAll("NonEmptyList[Int]", SemigroupTests[NonEmptyList[Int]].semigroup)
  checkAll("Semigroup[NonEmptyList[Int]]", SerializableTests.serializable(Semigroup[NonEmptyList[Int]]))

  checkAll("NonEmptyList[Int]", BimonadTests[NonEmptyList].bimonad[Int, Int, Int])
  checkAll("Bimonad[NonEmptyList]", SerializableTests.serializable(Bimonad[NonEmptyList]))

  checkAll("NonEmptyList[ListWrapper[Int]]", EqTests[NonEmptyList[ListWrapper[Int]]].eqv)
  checkAll("Eq[NonEmptyList[ListWrapper[Int]]]", SerializableTests.serializable(Eq[NonEmptyList[ListWrapper[Int]]]))

  checkAll("NonEmptyList[Int]", AlignTests[NonEmptyList].align[Int, Int, Int, Int])
  checkAll("Align[NonEmptyList]", SerializableTests.serializable(Align[NonEmptyList]))

  checkAll("ZipNonEmptyList[Int]", CommutativeApplyTests[ZipNonEmptyList].commutativeApply[Int, Int, Int])

  checkAll("NonEmptyList[Int]", ShortCircuitingTests[NonEmptyList].foldable[Int])
  checkAll("NonEmptyList[Int]", ShortCircuitingTests[NonEmptyList].traverse[Int])
  checkAll("NonEmptyList[Int]", ShortCircuitingTests[NonEmptyList].nonEmptyTraverse[Int])

  {
    implicit val A: PartialOrder[ListWrapper[Int]] = ListWrapper.partialOrder[Int]
    checkAll("NonEmptyList[ListWrapper[Int]]", PartialOrderTests[NonEmptyList[ListWrapper[Int]]].partialOrder)
    checkAll("PartialOrder[NonEmptyList[ListWrapper[Int]]]",
             SerializableTests.serializable(PartialOrder[NonEmptyList[ListWrapper[Int]]])
    )

    Eq[NonEmptyList[ListWrapper[Int]]]
  }

  {
    implicit val A: Order[ListWrapper[Int]] = ListWrapper.order[Int]
    checkAll("NonEmptyList[ListWrapper[Int]]", OrderTests[NonEmptyList[ListWrapper[Int]]].order)
    checkAll("Order[NonEmptyList[ListWrapper[Int]]]",
             SerializableTests.serializable(Order[NonEmptyList[ListWrapper[Int]]])
    )

    Eq[NonEmptyList[ListWrapper[Int]]]
    PartialOrder[NonEmptyList[ListWrapper[Int]]]
  }

  test("Show is not empty and is formatted as expected") {
    forAll { (nel: NonEmptyList[Int]) =>
      assert(nel.show.nonEmpty)
      assert(nel.show.startsWith("NonEmptyList(") === true)
      assert(nel.show === (implicitly[Show[NonEmptyList[Int]]].show(nel)))
      assert(nel.show.contains(nel.head.show) === true)
    }
  }

  test("Show is formatted correctly") {
    val nonEmptyList = NonEmptyList("Test", Nil)
    assert(nonEmptyList.show === "NonEmptyList(Test)")
  }

  test("Creating NonEmptyList + toList is identity") {
    forAll { (i: Int, tail: List[Int]) =>
      val list = i :: tail
      val nonEmptyList = NonEmptyList.of(i, tail: _*)
      assert(list === (nonEmptyList.toList))
    }
  }

  test("Creating NonEmptyList with init/last + toList is identity") {
    forAll { (init: List[Int], last: Int) =>
      val list = init :+ last
      val nonEmptyList = NonEmptyList.ofInitLast(init, last)
      assert(list === (nonEmptyList.toList))
    }
  }

  test("NonEmptyList#filter is consistent with List#filter") {
    forAll { (nel: NonEmptyList[Int], p: Int => Boolean) =>
      val list = nel.toList
      assert(nel.filter(p) === (list.filter(p)))
    }
  }

  test("NonEmptyList#filterNot is consistent with List#filterNot") {
    forAll { (nel: NonEmptyList[Int], p: Int => Boolean) =>
      val list = nel.toList
      assert(nel.filterNot(p) === (list.filterNot(p)))
    }
  }

  test("NonEmptyList#collect is consistent with List#collect") {
    forAll { (nel: NonEmptyList[Int], pf: PartialFunction[Int, String]) =>
      val list = nel.toList
      assert(nel.collect(pf) === (list.collect(pf)))
    }
  }

  test("NonEmptyList#find is consistent with List#find") {
    forAll { (nel: NonEmptyList[Int], p: Int => Boolean) =>
      val list = nel.toList
      assert(nel.find(p) === (list.find(p)))
    }
  }

  test("NonEmptyList#exists is consistent with List#exists") {
    forAll { (nel: NonEmptyList[Int], p: Int => Boolean) =>
      val list = nel.toList
      assert(nel.exists(p) === (list.exists(p)))
    }
  }

  test("NonEmptyList#forall is consistent with List#forall") {
    forAll { (nel: NonEmptyList[Int], p: Int => Boolean) =>
      val list = nel.toList
      assert(nel.forall(p) === (list.forall(p)))
    }
  }

  test("NonEmptyList#map is consistent with List#map") {
    forAll { (nel: NonEmptyList[Int], p: Int => String) =>
      val list = nel.toList
      assert(nel.map(p).toList === (list.map(p)))
    }
  }

  test("reduceLeft consistent with foldLeft") {
    forAll { (nel: NonEmptyList[Int], f: (Int, Int) => Int) =>
      assert(nel.reduceLeft(f) === (nel.tail.foldLeft(nel.head)(f)))
    }
  }

  test("reduceRight consistent with foldRight") {
    forAll { (nel: NonEmptyList[Int], f: (Int, Eval[Int]) => Eval[Int]) =>
      val got = nel.reduceRight(f).value
      nel.toList.reverse match {
        case last :: rev =>
          val expected = rev.reverse.foldRight(last)((a, b) => f(a, Now(b)).value)
          assert(got === expected)
        case _ => fail("nonempty turns out to be empty")
      }

    }
  }

  test("reduce consistent with fold") {
    forAll { (nel: NonEmptyList[Int]) =>
      assert(nel.reduce === (nel.fold))
    }
  }

  test("reduce consistent with reduceK") {
    forAll { (nel: NonEmptyList[Option[Int]]) =>
      assert(nel.reduce(SemigroupK[Option].algebra[Int]) === (nel.reduceK))
    }
  }

  test("reduceLeftToOption consistent with foldLeft + Option") {
    forAll { (nel: NonEmptyList[Int], f: Int => String, g: (String, Int) => String) =>
      val expected = nel.tail.foldLeft(Option(f(nel.head))) { (opt, i) =>
        opt.map(s => g(s, i))
      }
      assert(nel.reduceLeftToOption(f)(g) === expected)
    }
  }

  test("reduceRightToOption consistent with foldRight + Option") {
    forAll { (nel: NonEmptyList[Int], f: Int => String, g: (Int, Eval[String]) => Eval[String]) =>
      val got = nel.reduceRightToOption(f)(g).value
      nel.toList.reverse match {
        case last :: rev =>
          val expected = rev.reverse.foldRight(Option(f(last))) { (i, opt) =>
            opt.map(s => g(i, Now(s)).value)
          }
          assert(got === expected)
        case _ => fail("nonempty turns out to be empty")
      }

    }
  }

  test("reduceLeftM consistent with foldM") {
    forAll { (nel: NonEmptyList[Int], f: Int => Option[Int]) =>
      val got = nel.reduceLeftM(f)((acc, i) => f(i).map(acc + _))
      val expected = f(nel.head).flatMap { hd =>
        nel.tail.foldM(hd)((acc, i) => f(i).map(acc + _))
      }
      assert(got === expected)
    }
  }

  test("reduceMapM consistent with foldMapM") {
    forAll { (nel: NonEmptyList[Int], f: Int => Option[Int]) =>
      assert(nel.reduceMapM(f) === (nel.foldMapM(f)))
    }
  }

  test("fromList round trip") {
    forAll { (l: List[Int]) =>
      assert(NonEmptyList.fromList(l).map(_.toList).getOrElse(List.empty) === l)
    }

    forAll { (nel: NonEmptyList[Int]) =>
      assert(NonEmptyList.fromList(nel.toList) === (Some(nel)))
    }
  }

  test("fromListUnsafe/fromList consistency") {
    forAll { (nel: NonEmptyList[Int]) =>
      assert(NonEmptyList.fromList(nel.toList) === (Some(NonEmptyList.fromListUnsafe(nel.toList))))
    }
  }

  test("fromListUnsafe empty list") {
    val _ = intercept[IllegalArgumentException] {
      NonEmptyList.fromListUnsafe(List.empty[Int])
    }
  }

  test(":: consistent with List") {
    forAll { (nel: NonEmptyList[Int], i: Int) =>
      assert((i :: nel).toList === (i :: nel.toList))
      assert(nel.prepend(i).toList === (i :: nel.toList))
    }
  }

  test("++: consistent with List#:::") {
    forAll { (nel: NonEmptyList[Int], i: List[Int]) =>
      assert((i ++: nel).toList === (i ::: nel.toList))
      assert(nel.prependList(i).toList === (i ::: nel.toList))
    }
  }

  test("NonEmptyList#distinct is consistent with List#distinct") {
    forAll { (nel: NonEmptyList[Int]) =>
      assert(nel.distinct.toList === (nel.toList.distinct))
    }
  }

  test("NonEmptyList#reverse is consistent with List#reverse") {
    forAll { (nel: NonEmptyList[Int]) =>
      assert(nel.reverse.toList === (nel.toList.reverse))
    }
  }

  test("NonEmptyList#zipWithIndex is consistent with List#zipWithIndex") {
    forAll { (nel: NonEmptyList[Int]) =>
      assert(nel.zipWithIndex.toList === (nel.toList.zipWithIndex))
    }
  }

  test("NonEmptyList#last is consistent with List#last") {
    forAll { (nel: NonEmptyList[Int]) =>
      assert(nel.last === (nel.toList.last))
    }
  }

  test("NonEmptyList#init is consistent with List#init") {
    forAll { (nel: NonEmptyList[Int]) =>
      assert(nel.init === (nel.toList.init))
    }
  }

  test("NonEmptyList#take is consistent with List#take") {
    forAll { (n: Int, head: Int, tail: List[Int]) =>
      val list = head :: tail
      val nonEmptyList = NonEmptyList.of(head, tail: _*)
      assert(nonEmptyList.take(n) === list.take(n))
    }
  }

  test("NonEmptyList#size and length is consistent with List#size") {
    forAll { (nel: NonEmptyList[Int]) =>
      assert(nel.size === (nel.toList.size))
      assert(nel.length === (nel.toList.size))
    }
  }

  test("NonEmptyList#sorted is consistent with List#sorted") {
    forAll { (nel: NonEmptyList[Int]) =>
      assert(nel.sorted.toList === (nel.toList.sorted))
    }
  }

  test("NonEmptyList#sortBy is consistent with List#sortBy") {
    forAll { (nel: NonEmptyList[Int], f: Int => Int) =>
      assert(nel.sortBy(f).toList === (nel.toList.sortBy(f)))
    }
  }

  test("NonEmptyList#groupBy is consistent with List#groupBy") {
    forAll { (nel: NonEmptyList[Int], key: Int => Int) =>
      val result = nel.groupBy(key).map { case (k, v) => (k, v.toList) }.toMap
      val expected = nel.toList.groupBy(key)
      assert(result === expected)
    }
  }

  test("NonEmptyList#groupMap is consistent with List#groupBy + Map#mapValues") {
    forAll { (nel: NonEmptyList[Int], key: Int => Int, f: Int => String) =>
      val result = nel.groupMap(key)(f).map { case (k, v) => (k, v.toList) }.toMap
      val expected = nel.toList.groupBy(key).map { case (k, v) => (k, v.map(f)) }
      assert(result === expected)
    }
  }

  test("NonEmptyList#groupMapReduce is consistent with List#groupBy + Map#mapValues + List#reduce") {
    forAll { (nel: NonEmptyList[Int], key: Int => Int, f: Int => String) =>
      val result = nel.groupMapReduce(key)(f).toMap
      val expected = nel.toList.groupBy(key).map { case (k, v) => (k, v.map(f).reduce(Semigroup[String].combine)) }
      assert(result === expected)
    }
  }

  test("NonEmptyList#groupMapReduceWith is consistent with List#groupBy + Map#mapValues + List#reduce") {
    forAll { (nel: NonEmptyList[Int], key: Int => Int, f: Int => String, combine: (String, String) => String) =>
      val result = nel.groupMapReduceWith(key)(f)(combine).toMap
      val expected = nel.toList.groupBy(key).map { case (k, v) => (k, v.map(f).reduce(combine)) }
      assert(result === expected)
    }
  }

  test("NonEmptyList#concat/concatNel is consistent with List#:::") {
    forAll { (nel: NonEmptyList[Int], l: List[Int], n: Int) =>
      assert((nel ++ l).toList === (nel.toList ::: l))
      assert(nel.concat(l).toList === (nel.toList ::: l))
      assert(nel.appendList(l).toList === (nel.toList ::: l))
      assert(nel.concatNel(NonEmptyList(n, l)).toList === (nel.toList ::: (n :: l)))
    }
  }

  test("NonEmptyList#fromFoldabale is consistent with NonEmptyList#fromList") {
    forAll { (xs: List[Int]) =>
      assert(NonEmptyList.fromList(xs) === (NonEmptyList.fromFoldable(xs)))
    }
  }

  test("NonEmptyList#fromReducible is consistent with Reducible#toNonEmptyList") {
    forAll { (xs: NonEmptyVector[Int]) =>
      assert(NonEmptyList.fromReducible(xs) === (Reducible[NonEmptyVector].toNonEmptyList(xs)))
    }
  }

  test("NonEmptyList#zip is consistent with List#zip") {
    forAll { (a: NonEmptyList[Int], b: NonEmptyList[Int], f: (Int, Int) => Int) =>
      assert(a.zip(b).toList === (a.toList.zip(b.toList)))
    }
  }

  test("NonEmptyList#zipWith is consistent with List#zip and then List#map") {
    forAll { (a: NonEmptyList[Int], b: NonEmptyList[Int], f: (Int, Int) => Int) =>
      assert(a.zipWith(b)(f).toList === a.toList.zip(b.toList).map { case (x, y) => f(x, y) })
    }
  }

  test("NonEmptyList#nonEmptyPartition remains sorted") {
    forAll { (nel: NonEmptyList[Int], f: Int => Either[String, String]) =>
      val sorted = nel.map(f).sorted
      val ior = Reducible[NonEmptyList].nonEmptyPartition(sorted)(identity)

      assert(ior.left.forall(xs => xs.sorted === xs))
      assert(ior.right.forall(xs => xs.sorted === xs))
    }
  }

  test("NonEmptyList#toNem is consistent with List#toMap and creating NonEmptyMap from it") {
    forAll { (nel: NonEmptyList[(Int, String)]) =>
      assert(nel.toNem === (NonEmptyMap.fromMapUnsafe(SortedMap.empty[Int, String] ++ nel.toList.toMap)))
    }
  }

  test("NonEmptyList#toNes is consistent with List#toSet and creating NonEmptySet from it") {
    forAll { (nel: NonEmptyList[Int]) =>
      assert(nel.toNes === (NonEmptySet.fromSetUnsafe(SortedSet.empty[Int] ++ nel.toList.toSet)))
    }
  }

  test("NonEmptyList#toNev is consistent with List#toVector and creating NonEmptyVector from it") {
    forAll { (nel: NonEmptyList[Int]) =>
      assert(nel.toNev === (NonEmptyVector.fromVectorUnsafe(Vector.empty[Int] ++ nel.toList.toVector)))
    }
  }
}

@deprecated("to be able to test deprecated methods", since = "1.0.0-RC1")
class DeprecatedNonEmptyListSuite extends CatsSuite {

  test("Deprecated NonEmptyList#concat is consistent with List#:::") {
    forAll { (nel: NonEmptyList[Int], l: List[Int], n: Int) =>
      assert(nel.concatNel(NonEmptyList(n, l)).toList === (nel.toList ::: (n :: l)))
    }
  }
}

class ReducibleNonEmptyListSuite extends ReducibleSuite[NonEmptyList]("NonEmptyList") {
  def iterator[T](nel: NonEmptyList[T]): Iterator[T] = nel.toList.iterator

  def range(start: Long, endInclusive: Long): NonEmptyList[Long] = {
    // if we inline this we get a bewildering implicit numeric widening
    // error message in Scala 2.10
    val tailStart: Long = start + 1L
    NonEmptyList(start, tailStart.to(endInclusive).toList)
  }

  def fromValues[A](el: A, els: A*): NonEmptyList[A] = NonEmptyList(el, List(els: _*))
}
