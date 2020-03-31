package cats.tests

import cats.{Align, Bimonad, Eval, NonEmptyTraverse, Now, Reducible, SemigroupK, Show}
import cats.data.{NonEmptyList, NonEmptyMap, NonEmptySet, NonEmptyVector}
import cats.data.NonEmptyList.ZipNonEmptyList
import cats.kernel.{Eq, Order, PartialOrder, Semigroup}
import cats.kernel.laws.discipline.{EqTests, OrderTests, PartialOrderTests, SemigroupTests}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{
  AlignTests,
  BimonadTests,
  CommutativeApplyTests,
  NonEmptyTraverseTests,
  ReducibleTests,
  SemigroupKTests,
  SerializableTests,
  ShortCircuitingTests
}
import cats.syntax.foldable._
import cats.syntax.reducible._
import cats.syntax.show._
import scala.collection.immutable.{SortedMap, SortedSet}

class NonEmptyListSuite extends NonEmptyCollectionSuite[List, NonEmptyList, NonEmptyList] {
  protected def toList[A](value: NonEmptyList[A]): List[A] = value.toList
  protected def underlyingToList[A](underlying: List[A]): List[A] = underlying
  protected def toNonEmptyCollection[A](nea: NonEmptyList[A]): NonEmptyList[A] = nea

  // Lots of collections here.. telling ScalaCheck to calm down a bit
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 20, sizeRange = 5)

  checkAll("NonEmptyList[Int]", OrderTests[NonEmptyList[Int]].order)

  checkAll("NonEmptyList[Int] with Option",
           NonEmptyTraverseTests[NonEmptyList].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option])
  checkAll("NonEmptyTraverse[NonEmptyList[A]]", SerializableTests.serializable(NonEmptyTraverse[NonEmptyList]))

  checkAll("NonEmptyList[Int]", ReducibleTests[NonEmptyList].reducible[Option, Int, Int])
  checkAll("Reducible[NonEmptyList]", SerializableTests.serializable(Reducible[NonEmptyList]))

  checkAll("NonEmptyList[Int]", SemigroupKTests[NonEmptyList].semigroupK[Int])
  checkAll("SemigroupK[NonEmptyList[A]]", SerializableTests.serializable(SemigroupK[NonEmptyList]))

  checkAll("NonEmptyList[Int]", SemigroupTests[NonEmptyList[Int]].semigroup)
  checkAll("Semigroup[NonEmptyList[Int]]", SerializableTests.serializable(Semigroup[NonEmptyList[Int]]))

  checkAll("NonEmptyList[Int]", BimonadTests[NonEmptyList].bimonad[Int, Int, Int])
  checkAll("Bimonad[NonEmptyList]", SerializableTests.serializable(Bimonad[NonEmptyList]))

  checkAll("NonEmptyList[ListWrapper[Int]]", EqTests[NonEmptyList[ListWrapper[Int]]].eqv)
  checkAll("Eq[NonEmptyList[ListWrapper[Int]]]", SerializableTests.serializable(Eq[NonEmptyList[ListWrapper[Int]]]))

  checkAll("NonEmptyList[Int]", AlignTests[NonEmptyList].align[Int, Int, Int, Int])
  checkAll("Align[NonEmptyList]", SerializableTests.serializable(Align[NonEmptyList]))

  checkAll("ZipNonEmptyList[Int]", CommutativeApplyTests[ZipNonEmptyList].commutativeApply[Int, Int, Int])

  checkAll("NonEmptyList[Int]", ShortCircuitingTests[NonEmptyList].traverse[Int])

  {
    implicit val A: PartialOrder[ListWrapper[Int]] = ListWrapper.partialOrder[Int]
    checkAll("NonEmptyList[ListWrapper[Int]]", PartialOrderTests[NonEmptyList[ListWrapper[Int]]].partialOrder)
    checkAll("PartialOrder[NonEmptyList[ListWrapper[Int]]]",
             SerializableTests.serializable(PartialOrder[NonEmptyList[ListWrapper[Int]]]))

    Eq[NonEmptyList[ListWrapper[Int]]]
  }

  {
    implicit val A: Order[ListWrapper[Int]] = ListWrapper.order[Int]
    checkAll("NonEmptyList[ListWrapper[Int]]", OrderTests[NonEmptyList[ListWrapper[Int]]].order)
    checkAll("Order[NonEmptyList[ListWrapper[Int]]]",
             SerializableTests.serializable(Order[NonEmptyList[ListWrapper[Int]]]))

    Eq[NonEmptyList[ListWrapper[Int]]]
    PartialOrder[NonEmptyList[ListWrapper[Int]]]
  }

  test("Show is not empty and is formatted as expected") {
    forAll { (nel: NonEmptyList[Int]) =>
      nel.show.nonEmpty should ===(true)
      nel.show.startsWith("NonEmptyList(") should ===(true)
      nel.show should ===(implicitly[Show[NonEmptyList[Int]]].show(nel))
      nel.show.contains(nel.head.show) should ===(true)
    }
  }

  test("Show is formatted correctly") {
    val nonEmptyList = NonEmptyList("Test", Nil)
    nonEmptyList.show should ===("NonEmptyList(Test)")
  }

  test("Creating NonEmptyList + toList is identity") {
    forAll { (i: Int, tail: List[Int]) =>
      val list = i :: tail
      val nonEmptyList = NonEmptyList.of(i, tail: _*)
      list should ===(nonEmptyList.toList)
    }
  }

  test("Creating NonEmptyList with init/last + toList is identity") {
    forAll { (init: List[Int], last: Int) =>
      val list = init :+ last
      val nonEmptyList = NonEmptyList.ofInitLast(init, last)
      list should ===(nonEmptyList.toList)
    }
  }

  test("NonEmptyList#filter is consistent with List#filter") {
    forAll { (nel: NonEmptyList[Int], p: Int => Boolean) =>
      val list = nel.toList
      nel.filter(p) should ===(list.filter(p))
    }
  }

  test("NonEmptyList#filterNot is consistent with List#filterNot") {
    forAll { (nel: NonEmptyList[Int], p: Int => Boolean) =>
      val list = nel.toList
      nel.filterNot(p) should ===(list.filterNot(p))
    }
  }

  test("NonEmptyList#collect is consistent with List#collect") {
    forAll { (nel: NonEmptyList[Int], pf: PartialFunction[Int, String]) =>
      val list = nel.toList
      nel.collect(pf) should ===(list.collect(pf))
    }
  }

  test("NonEmptyList#find is consistent with List#find") {
    forAll { (nel: NonEmptyList[Int], p: Int => Boolean) =>
      val list = nel.toList
      nel.find(p) should ===(list.find(p))
    }
  }

  test("NonEmptyList#exists is consistent with List#exists") {
    forAll { (nel: NonEmptyList[Int], p: Int => Boolean) =>
      val list = nel.toList
      nel.exists(p) should ===(list.exists(p))
    }
  }

  test("NonEmptyList#forall is consistent with List#forall") {
    forAll { (nel: NonEmptyList[Int], p: Int => Boolean) =>
      val list = nel.toList
      nel.forall(p) should ===(list.forall(p))
    }
  }

  test("NonEmptyList#map is consistent with List#map") {
    forAll { (nel: NonEmptyList[Int], p: Int => String) =>
      val list = nel.toList
      nel.map(p).toList should ===(list.map(p))
    }
  }

  test("reduceLeft consistent with foldLeft") {
    forAll { (nel: NonEmptyList[Int], f: (Int, Int) => Int) =>
      nel.reduceLeft(f) should ===(nel.tail.foldLeft(nel.head)(f))
    }
  }

  test("reduceRight consistent with foldRight") {
    forAll { (nel: NonEmptyList[Int], f: (Int, Eval[Int]) => Eval[Int]) =>
      val got = nel.reduceRight(f).value
      nel.toList.reverse match {
        case last :: rev =>
          val expected = rev.reverse.foldRight(last)((a, b) => f(a, Now(b)).value)
          got should ===(expected)
        case _ => fail("nonempty turns out to be empty")
      }

    }
  }

  test("reduce consistent with fold") {
    forAll { (nel: NonEmptyList[Int]) =>
      nel.reduce should ===(nel.fold)
    }
  }

  test("reduce consistent with reduceK") {
    forAll { (nel: NonEmptyList[Option[Int]]) =>
      nel.reduce(SemigroupK[Option].algebra[Int]) should ===(nel.reduceK)
    }
  }

  test("reduceLeftToOption consistent with foldLeft + Option") {
    forAll { (nel: NonEmptyList[Int], f: Int => String, g: (String, Int) => String) =>
      val expected = nel.tail.foldLeft(Option(f(nel.head))) { (opt, i) =>
        opt.map(s => g(s, i))
      }
      nel.reduceLeftToOption(f)(g) should ===(expected)
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
          got should ===(expected)
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
      got should ===(expected)
    }
  }

  test("reduceMapM consistent with foldMapM") {
    forAll { (nel: NonEmptyList[Int], f: Int => Option[Int]) =>
      nel.reduceMapM(f) should ===(nel.foldMapM(f))
    }
  }

  test("fromList round trip") {
    forAll { (l: List[Int]) =>
      NonEmptyList.fromList(l).map(_.toList).getOrElse(List.empty) should ===(l)
    }

    forAll { (nel: NonEmptyList[Int]) =>
      NonEmptyList.fromList(nel.toList) should ===(Some(nel))
    }
  }

  test("fromListUnsafe/fromList consistency") {
    forAll { (nel: NonEmptyList[Int]) =>
      NonEmptyList.fromList(nel.toList) should ===(Some(NonEmptyList.fromListUnsafe(nel.toList)))
    }
  }

  test("fromListUnsafe empty list") {
    val _ = intercept[IllegalArgumentException] {
      NonEmptyList.fromListUnsafe(List.empty[Int])
    }
  }

  test(":: consistent with List") {
    forAll { (nel: NonEmptyList[Int], i: Int) =>
      (i :: nel).toList should ===(i :: nel.toList)
      nel.prepend(i).toList should ===(i :: nel.toList)
    }
  }

  test("NonEmptyList#distinct is consistent with List#distinct") {
    forAll { (nel: NonEmptyList[Int]) =>
      nel.distinct.toList should ===(nel.toList.distinct)
    }
  }

  test("NonEmptyList#reverse is consistent with List#reverse") {
    forAll { (nel: NonEmptyList[Int]) =>
      nel.reverse.toList should ===(nel.toList.reverse)
    }
  }

  test("NonEmptyList#zipWithIndex is consistent with List#zipWithIndex") {
    forAll { (nel: NonEmptyList[Int]) =>
      nel.zipWithIndex.toList should ===(nel.toList.zipWithIndex)
    }
  }

  test("NonEmptyList#last is consistent with List#last") {
    forAll { (nel: NonEmptyList[Int]) =>
      nel.last should ===(nel.toList.last)
    }
  }

  test("NonEmptyList#init is consistent with List#init") {
    forAll { (nel: NonEmptyList[Int]) =>
      nel.init should ===(nel.toList.init)
    }
  }

  test("NonEmptyList#size and length is consistent with List#size") {
    forAll { (nel: NonEmptyList[Int]) =>
      nel.size should ===(nel.toList.size)
      nel.length should ===(nel.toList.size)
    }
  }

  test("NonEmptyList#sorted is consistent with List#sorted") {
    forAll { (nel: NonEmptyList[Int]) =>
      nel.sorted.toList should ===(nel.toList.sorted)
    }
  }

  test("NonEmptyList#sortBy is consistent with List#sortBy") {
    forAll { (nel: NonEmptyList[Int], f: Int => Int) =>
      nel.sortBy(f).toList should ===(nel.toList.sortBy(f))
    }
  }

  test("NonEmptyList#groupBy is consistent with List#groupBy") {
    forAll { (nel: NonEmptyList[Int], f: Int => Int) =>
      nel.groupBy(f).map { case (k, v) => (k, v.toList) } should ===(nel.toList.groupBy(f))
    }
  }

  test("NonEmptyList#concat/concatNel is consistent with List#:::") {
    forAll { (nel: NonEmptyList[Int], l: List[Int], n: Int) =>
      (nel ++ l).toList should ===(nel.toList ::: l)
      nel.concat(l).toList should ===(nel.toList ::: l)
      nel.concatNel(NonEmptyList(n, l)).toList should ===(nel.toList ::: (n :: l))
    }
  }

  test("NonEmptyList#fromFoldabale is consistent with NonEmptyList#fromList") {
    forAll { (xs: List[Int]) =>
      NonEmptyList.fromList(xs) should ===(NonEmptyList.fromFoldable(xs))
    }
  }

  test("NonEmptyList#fromReducible is consistent with Reducible#toNonEmptyList") {
    forAll { (xs: NonEmptyVector[Int]) =>
      NonEmptyList.fromReducible(xs) should ===(Reducible[NonEmptyVector].toNonEmptyList(xs))
    }
  }

  test("NonEmptyList#zipWith is consistent with List#zip and then List#map") {
    forAll { (a: NonEmptyList[Int], b: NonEmptyList[Int], f: (Int, Int) => Int) =>
      a.zipWith(b)(f).toList should ===(a.toList.zip(b.toList).map { case (x, y) => f(x, y) })
    }
  }
  test("NonEmptyList#nonEmptyPartition remains sorted") {
    forAll { (nel: NonEmptyList[Int], f: Int => Either[String, String]) =>
      val sorted = nel.map(f).sorted
      val ior = Reducible[NonEmptyList].nonEmptyPartition(sorted)(identity)

      ior.left.map(xs => xs.sorted should ===(xs))
      ior.right.map(xs => xs.sorted should ===(xs))
    }
  }

  test("NonEmptyList#toNem is consistent with List#toMap and creating NonEmptyMap from it") {
    forAll { (nel: NonEmptyList[(Int, String)]) =>
      nel.toNem should ===(NonEmptyMap.fromMapUnsafe(SortedMap.empty[Int, String] ++ nel.toList.toMap))
    }
  }

  test("NonEmptyList#toNes is consistent with List#toSet and creating NonEmptySet from it") {
    forAll { (nel: NonEmptyList[Int]) =>
      nel.toNes should ===(NonEmptySet.fromSetUnsafe(SortedSet.empty[Int] ++ nel.toList.toSet))
    }
  }
}

@deprecated("to be able to test deprecated methods", since = "1.0.0-RC1")
class DeprecatedNonEmptyListSuite extends CatsSuite {

  test("Deprecated NonEmptyList#concat is consistent with List#:::") {
    forAll { (nel: NonEmptyList[Int], l: List[Int], n: Int) =>
      nel.concatNel(NonEmptyList(n, l)).toList should ===(nel.toList ::: (n :: l))
    }
  }
}

class ReducibleNonEmptyListSuite extends ReducibleSuite[NonEmptyList]("NonEmptyList") {
  def iterator[T](nel: NonEmptyList[T]): Iterator[T] = nel.toList.iterator

  def range(start: Long, endInclusive: Long): NonEmptyList[Long] = {
    // if we inline this we get a bewildering implicit numeric widening
    // error message in Scala 2.10
    val tailStart: Long = start + 1L
    NonEmptyList(start, (tailStart).to(endInclusive).toList)
  }

  def fromValues[A](el: A, els: A*): NonEmptyList[A] = NonEmptyList(el, List(els: _*))
}
