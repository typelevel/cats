package cats
package tests

import algebra.laws.{GroupLaws, OrderLaws}

import cats.data.{NonEmptyList, OneAnd}
import cats.laws.discipline.{ComonadTests, FunctorTests, SemigroupKTests, FoldableTests, MonadTests, SerializableTests}
import cats.laws.discipline.arbitrary.{evalArbitrary, oneAndArbitrary}


import scala.util.Random

class OneAndTests extends CatsSuite {
  checkAll("OneAnd[List, Int]", OrderLaws[OneAnd[List, Int]].eqv)

  // Test instances that have more general constraints
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

  checkAll("NonEmptyList[Int]", MonadTests[NonEmptyList].monad[Int, Int, Int])
  checkAll("Monad[NonEmptyList[A]]", SerializableTests.serializable(Monad[NonEmptyList]))

  checkAll("NonEmptyList[Int]", ComonadTests[NonEmptyList].comonad[Int, Int, Int])
  checkAll("Comonad[NonEmptyList[A]]", SerializableTests.serializable(Comonad[NonEmptyList]))

  test("Creating OneAnd + unwrap is identity") {
    forAll { (list: List[Int]) =>
      whenever(list.size >= 1) {
        val oneAnd = NonEmptyList(list.head, list.tail: _*)
        list should === (oneAnd.unwrap)
      }
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
}
