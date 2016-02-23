package cats
package tests

import algebra.laws.{GroupLaws, OrderLaws}

import cats.data.{NonEmptyList, OneAnd}
import cats.laws.discipline.{ComonadTests, FunctorTests, SemigroupKTests, FoldableTests, MonadTests, SerializableTests, CartesianTests, TraverseTests}
import cats.laws.discipline.arbitrary.{evalArbitrary, oneAndArbitrary}
import cats.laws.discipline.eq._

import scala.util.Random

class OneAndTests extends CatsSuite {
  checkAll("OneAnd[List, Int]", OrderLaws[OneAnd[List, Int]].eqv)

  checkAll("OneAnd[List, Int] with Option", TraverseTests[OneAnd[List, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[OneAnd[List, A]]", SerializableTests.serializable(Traverse[OneAnd[List, ?]]))

  implicit val iso = CartesianTests.Isomorphisms.invariant[OneAnd[ListWrapper, ?]](OneAnd.oneAndFunctor(ListWrapper.functor))

  // Test instances that have more general constraints
  {
    implicit val monadCombine = ListWrapper.monadCombine
    checkAll("OneAnd[ListWrapper, Int]", CartesianTests[OneAnd[ListWrapper, ?]].cartesian[Int, Int, Int])
    checkAll("Cartesian[OneAnd[ListWrapper, A]]", SerializableTests.serializable(Cartesian[OneAnd[ListWrapper, ?]]))
  }

  {
    implicit val functor = ListWrapper.functor
    checkAll("OneAnd[ListWrapper, Int]", FunctorTests[OneAnd[ListWrapper, ?]].functor[Int, Int, Int])
    checkAll("Functor[OneAnd[ListWrapper, A]]", SerializableTests.serializable(Functor[OneAnd[ListWrapper, ?]]))
  }

  {
    implicit val monadCombine = ListWrapper.monadCombine
    checkAll("OneAnd[ListWrapper, Int]", SemigroupKTests[OneAnd[ListWrapper, ?]].semigroupK[Int])
    checkAll("OneAnd[List, Int]", GroupLaws[OneAnd[List, Int]].semigroup)
    checkAll("SemigroupK[OneAnd[ListWrapper, A]]", SerializableTests.serializable(SemigroupK[OneAnd[ListWrapper, ?]]))
    checkAll("Semigroup[NonEmptyList[Int]]", SerializableTests.serializable(Semigroup[OneAnd[List, Int]]))
  }

  {
    implicit val foldable = ListWrapper.foldable
    checkAll("OneAnd[ListWrapper, Int]", FoldableTests[OneAnd[ListWrapper, ?]].foldable[Int, Int])
    checkAll("Foldable[OneAnd[ListWrapper, A]]", SerializableTests.serializable(Foldable[OneAnd[ListWrapper, ?]]))
  }

  {
    // Test functor and subclasses don't have implicit conflicts
    implicitly[Functor[NonEmptyList]]
    implicitly[Monad[NonEmptyList]]
    implicitly[Comonad[NonEmptyList]]
  }

  implicit val iso2 = CartesianTests.Isomorphisms.invariant[OneAnd[List, ?]]

  checkAll("NonEmptyList[Int]", MonadTests[NonEmptyList].monad[Int, Int, Int])
  checkAll("Monad[NonEmptyList[A]]", SerializableTests.serializable(Monad[NonEmptyList]))

  checkAll("NonEmptyList[Int]", ComonadTests[NonEmptyList].comonad[Int, Int, Int])
  checkAll("Comonad[NonEmptyList[A]]", SerializableTests.serializable(Comonad[NonEmptyList]))

  test("Show is not empty and is formatted as expected") {
    forAll { (nel: NonEmptyList[Int]) =>
      nel.show.nonEmpty should === (true)
      nel.show.startsWith("OneAnd(") should === (true)
      nel.show should === (implicitly[Show[NonEmptyList[Int]]].show(nel))
      nel.show.contains(nel.head.show) should === (true)
    }
  }

  test("Show is formatted correctly") {
    val oneAnd = NonEmptyList("Test", Nil)
    oneAnd.show should === ("OneAnd(Test, List())")
  }

  test("Creating OneAnd + unwrap is identity") {
    forAll { (i: Int, tail: List[Int]) =>
      val list = i :: tail
      val oneAnd = NonEmptyList(i, tail: _*)
      list should === (oneAnd.unwrap)
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
}
