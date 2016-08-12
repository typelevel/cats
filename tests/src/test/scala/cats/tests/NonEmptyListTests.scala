package cats
package tests

import cats.kernel.laws.{GroupLaws, OrderLaws}

import cats.data.NonEmptyList
import cats.laws.discipline.{ComonadTests, SemigroupKTests, MonadTests, SerializableTests, TraverseTests, ReducibleTests}
import cats.laws.discipline.arbitrary._

class NonEmptyListTests extends CatsSuite {
  // Lots of collections here.. telling ScalaCheck to calm down a bit
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(maxSize = 5, minSuccessful = 20)

  checkAll("NonEmptyList[Int]", OrderLaws[NonEmptyList[Int]].order)

  checkAll("NonEmptyList[Int] with Option", TraverseTests[NonEmptyList].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[NonEmptyList[A]]", SerializableTests.serializable(Traverse[NonEmptyList]))

  checkAll("NonEmptyList[Int]", ReducibleTests[NonEmptyList].reducible[Option, Int, Int])
  checkAll("Reducible[NonEmptyList]", SerializableTests.serializable(Reducible[NonEmptyList]))

  checkAll("NonEmptyList[Int]", MonadTests[NonEmptyList].monad[Int, Int, Int])
  checkAll("Monad[NonEmptyList[A]]", SerializableTests.serializable(Monad[NonEmptyList]))

  checkAll("NonEmptyList[Int]", SemigroupKTests[NonEmptyList].semigroupK[Int])
  checkAll("SemigroupK[NonEmptyList[A]]", SerializableTests.serializable(SemigroupK[NonEmptyList]))

  checkAll("NonEmptyList[Int]", GroupLaws[NonEmptyList[Int]].semigroup)
  checkAll("Semigroup[NonEmptyList[Int]]", SerializableTests.serializable(Semigroup[NonEmptyList[Int]]))

  checkAll("NonEmptyList[Int]", ComonadTests[NonEmptyList].comonad[Int, Int, Int])
  checkAll("Comonad[NonEmptyList]", SerializableTests.serializable(Comonad[NonEmptyList]))

  checkAll("NonEmptyList[ListWrapper[Int]]", OrderLaws[NonEmptyList[ListWrapper[Int]]].eqv)
  checkAll("Eq[NonEmptyList[ListWrapper[Int]]]", SerializableTests.serializable(Eq[NonEmptyList[ListWrapper[Int]]]))

  {
    implicit val A = ListWrapper.partialOrder[Int]
    checkAll("NonEmptyList[ListWrapper[Int]]", OrderLaws[NonEmptyList[ListWrapper[Int]]].partialOrder)
    checkAll("PartialOrder[NonEmptyList[ListWrapper[Int]]]", SerializableTests.serializable(PartialOrder[NonEmptyList[ListWrapper[Int]]]))

    Eq[NonEmptyList[ListWrapper[Int]]]
  }

  {
    implicit val A = ListWrapper.order[Int]
    checkAll("NonEmptyList[ListWrapper[Int]]", OrderLaws[NonEmptyList[ListWrapper[Int]]].order)
    checkAll("Order[NonEmptyList[ListWrapper[Int]]]", SerializableTests.serializable(Order[NonEmptyList[ListWrapper[Int]]]))

    Eq[NonEmptyList[ListWrapper[Int]]]
    PartialOrder[NonEmptyList[ListWrapper[Int]]]
  }

  test("Show is not empty and is formatted as expected") {
    forAll { (nel: NonEmptyList[Int]) =>
      nel.show.nonEmpty should === (true)
      nel.show.startsWith("NonEmptyList(") should === (true)
      nel.show should === (implicitly[Show[NonEmptyList[Int]]].show(nel))
      nel.show.contains(nel.head.show) should === (true)
    }
  }

  test("Show is formatted correctly") {
    val nonEmptyList = NonEmptyList("Test", Nil)
    nonEmptyList.show should === ("NonEmptyList(Test)")
  }

  test("Creating NonEmptyList + toList is identity") {
    forAll { (i: Int, tail: List[Int]) =>
      val list = i :: tail
      val nonEmptyList = NonEmptyList.of(i, tail: _*)
      list should === (nonEmptyList.toList)
    }
  }

  test("NonEmptyList#filter is consistent with List#filter") {
    forAll { (nel: NonEmptyList[Int], p: Int => Boolean) =>
      val list = nel.toList
      nel.filter(p) should === (list.filter(p))
    }
  }

  test("NonEmptyList#find is consistent with List#find") {
    forAll { (nel: NonEmptyList[Int], p: Int => Boolean) =>
      val list = nel.toList
      nel.find(p) should === (list.find(p))
    }
  }

  test("NonEmptyList#exists is consistent with List#exists") {
    forAll { (nel: NonEmptyList[Int], p: Int => Boolean) =>
      val list = nel.toList
      nel.exists(p) should === (list.exists(p))
    }
  }

  test("NonEmptyList#forall is consistent with List#forall") {
    forAll { (nel: NonEmptyList[Int], p: Int => Boolean) =>
      val list = nel.toList
      nel.forall(p) should === (list.forall(p))
    }
  }

  test("NonEmptyList#map is consistent with List#map") {
    forAll { (nel: NonEmptyList[Int], p: Int => String) =>
      val list = nel.toList
      nel.map(p).toList should === (list.map(p))
    }
  }

  test("reduceLeft consistent with foldLeft") {
    forAll { (nel: NonEmptyList[Int], f: (Int, Int) => Int) =>
      nel.reduceLeft(f) should === (nel.tail.foldLeft(nel.head)(f))
    }
  }

  test("reduceRight consistent with foldRight") {
    forAll { (nel: NonEmptyList[Int], f: (Int, Eval[Int]) => Eval[Int]) =>
      nel.reduceRight(f).value should === (nel.tail.foldRight(nel.head)((a, b) => f(a, Now(b)).value))
    }
  }

  test("reduce consistent with fold") {
    forAll { (nel: NonEmptyList[Int]) =>
      nel.reduce should === (nel.fold)
    }
  }

  test("reduce consistent with reduceK") {
    forAll { (nel: NonEmptyList[Option[Int]]) =>
      nel.reduce(SemigroupK[Option].algebra[Int]) should === (nel.reduceK)
    }
  }

  test("reduceLeftToOption consistent with foldLeft + Option") {
    forAll { (nel: NonEmptyList[Int], f: Int => String, g: (String, Int) => String) =>
      val expected = nel.tail.foldLeft(Option(f(nel.head))) { (opt, i) =>
        opt.map(s => g(s, i))
      }
      nel.reduceLeftToOption(f)(g) should === (expected)
    }
  }

  test("reduceRightToOption consistent with foldRight + Option") {
    forAll { (nel: NonEmptyList[Int], f: Int => String, g: (Int, Eval[String]) => Eval[String]) =>
      val expected = nel.tail.foldRight(Option(f(nel.head))) { (i, opt) =>
        opt.map(s => g(i, Now(s)).value)
      }
      nel.reduceRightToOption(f)(g).value should === (expected)
    }
  }

  test("fromList round trip") {
    forAll { l: List[Int] =>
      NonEmptyList.fromList(l).map(_.toList).getOrElse(List.empty) should === (l)
    }

    forAll { nel: NonEmptyList[Int] =>
      NonEmptyList.fromList(nel.toList) should === (Some(nel))
    }
  }

  test("fromListUnsafe/fromList consistency") {
    forAll { nel: NonEmptyList[Int] =>
      NonEmptyList.fromList(nel.toList) should === (Some(NonEmptyList.fromListUnsafe(nel.toList)))
    }
  }

  test("fromListUnsafe empty list") {
    val _ = intercept[IllegalArgumentException] {
      NonEmptyList.fromListUnsafe(List.empty[Int])
    }
  }

  test(":: consistent with List") {
    forAll { (nel: NonEmptyList[Int], i: Int) =>
      (i :: nel).toList should === (i :: nel.toList)
    }
  }

  test("NonEmptyList#distinct is consistent with List#distinct") {
    forAll { nel: NonEmptyList[Int] =>
      nel.distinct.toList should === (nel.toList.distinct)
    }
  }
}

class ReducibleNonEmptyListCheck extends ReducibleCheck[NonEmptyList]("NonEmptyList") {
  def iterator[T](nel: NonEmptyList[T]): Iterator[T] = nel.toList.iterator

  def range(start: Long, endInclusive: Long): NonEmptyList[Long] = {
    // if we inline this we get a bewildering implicit numeric widening
    // error message in Scala 2.10
    val tailStart: Long = start + 1L
    NonEmptyList(start, (tailStart).to(endInclusive).toList)
  }

}
