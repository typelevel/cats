package cats
package tests

import cats.kernel.laws.{GroupLaws, OrderLaws}

import cats.data.{NonEmptyList, NonEmptyList}
import cats.laws.discipline.{ComonadTests, FunctorTests, SemigroupKTests, FoldableTests, MonadTests, SerializableTests, CartesianTests, TraverseTests, ReducibleTests}
import cats.laws.discipline.arbitrary._

class NonEmptyListTests extends CatsSuite {
  // Lots of collections here.. telling ScalaCheck to calm down a bit
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(maxSize = 5, minSuccessful = 20)

  checkAll("NonEmptyList[List, Int]", OrderLaws[NonEmptyList[List, Int]].eqv)

  checkAll("NonEmptyList[List, Int] with Option", TraverseTests[NonEmptyList[List, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[NonEmptyList[List, A]]", SerializableTests.serializable(Traverse[NonEmptyList[List, ?]]))

  checkAll("NonEmptyList[List, Int]", ReducibleTests[NonEmptyList[List, ?]].reducible[Option, Int, Int])
  checkAll("Reducible[NonEmptyList[List, ?]]", SerializableTests.serializable(Reducible[NonEmptyList[List, ?]]))

  implicit val iso = CartesianTests.Isomorphisms.invariant[NonEmptyList[ListWrapper, ?]](NonEmptyList.catsDataFunctorForNonEmptyList(ListWrapper.functor))

  // Test instances that have more general constraints
  {
    implicit val monadCombine = ListWrapper.monadCombine
    checkAll("NonEmptyList[ListWrapper, Int]", CartesianTests[NonEmptyList[ListWrapper, ?]].cartesian[Int, Int, Int])
    checkAll("Cartesian[NonEmptyList[ListWrapper, A]]", SerializableTests.serializable(Cartesian[NonEmptyList[ListWrapper, ?]]))
  }

  {
    implicit val functor = ListWrapper.functor
    checkAll("NonEmptyList[ListWrapper, Int]", FunctorTests[NonEmptyList[ListWrapper, ?]].functor[Int, Int, Int])
    checkAll("Functor[NonEmptyList[ListWrapper, A]]", SerializableTests.serializable(Functor[NonEmptyList[ListWrapper, ?]]))
  }

  {
    implicit val monadCombine = ListWrapper.monadCombine
    checkAll("NonEmptyList[ListWrapper, Int]", SemigroupKTests[NonEmptyList[ListWrapper, ?]].semigroupK[Int])
    checkAll("NonEmptyList[List, Int]", GroupLaws[NonEmptyList[List, Int]].semigroup)
    checkAll("SemigroupK[NonEmptyList[ListWrapper, A]]", SerializableTests.serializable(SemigroupK[NonEmptyList[ListWrapper, ?]]))
    checkAll("Semigroup[NonEmptyList[Int]]", SerializableTests.serializable(Semigroup[NonEmptyList[List, Int]]))
  }

  {
    implicit val foldable = ListWrapper.foldable
    checkAll("NonEmptyList[ListWrapper, Int]", FoldableTests[NonEmptyList[ListWrapper, ?]].foldable[Int, Int])
    checkAll("Foldable[NonEmptyList[ListWrapper, A]]", SerializableTests.serializable(Foldable[NonEmptyList[ListWrapper, ?]]))
  }

  {
    // Test functor and subclasses don't have implicit conflicts
    implicitly[Functor[NonEmptyList]]
    implicitly[Monad[NonEmptyList]]
    implicitly[Comonad[NonEmptyList]]
  }

  implicit val iso2 = CartesianTests.Isomorphisms.invariant[NonEmptyList[List, ?]]

  checkAll("NonEmptyList[Int]", MonadTests[NonEmptyList].monad[Int, Int, Int])
  checkAll("Monad[NonEmptyList[A]]", SerializableTests.serializable(Monad[NonEmptyList]))

  checkAll("NonEmptyList[Int]", ComonadTests[NonEmptyList].comonad[Int, Int, Int])
  checkAll("Comonad[NonEmptyList[A]]", SerializableTests.serializable(Comonad[NonEmptyList]))

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
    nonEmptyList.show should === ("NonEmptyList(Test, List())")
  }

  test("Creating NonEmptyList + unwrap is identity") {
    forAll { (i: Int, tail: List[Int]) =>
      val list = i :: tail
      val nonEmptyList = NonEmptyList(i, tail: _*)
      list should === (nonEmptyList.unwrap)
    }
  }

  test("NonEmptyList#filter is consistent with List#filter") {
    forAll { (nel: NonEmptyList[Int], p: Int => Boolean) =>
      val list = nel.unwrap
      nel.filter(p) should === (list.filter(p))
    }
  }

  test("NonEmptyList#find is consistent with List#find") {
    forAll { (nel: NonEmptyList[Int], p: Int => Boolean) =>
      val list = nel.unwrap
      nel.find(p) should === (list.find(p))
    }
  }

  test("NonEmptyList#exists is consistent with List#exists") {
    forAll { (nel: NonEmptyList[Int], p: Int => Boolean) =>
      val list = nel.unwrap
      nel.exists(p) should === (list.exists(p))
    }
  }

  test("NonEmptyList#forall is consistent with List#forall") {
    forAll { (nel: NonEmptyList[Int], p: Int => Boolean) =>
      val list = nel.unwrap
      nel.forall(p) should === (list.forall(p))
    }
  }

  test("NonEmptyList#map is consistent with List#map") {
    forAll { (nel: NonEmptyList[Int], p: Int => String) =>
      val list = nel.unwrap
      nel.map(p).unwrap should === (list.map(p))
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
}
