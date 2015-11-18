package cats
package tests

import algebra.laws.{GroupLaws, OrderLaws}

import cats.data.{NonEmptyList, OneAnd}
import cats.laws.discipline.{ComonadTests, FunctorTests, SemigroupKTests, FoldableTests, MonadTests, SerializableTests, MonoidalTests}
import cats.laws.discipline.arbitrary.{evalArbitrary, oneAndArbitrary}
import cats.laws.discipline.eq._

import scala.util.Random

class OneAndTests extends CatsSuite {
  checkAll("OneAnd[Int, List]", OrderLaws[OneAnd[Int, List]].eqv)

  // Test instances that have more general constraints
  {
    implicit val monadCombine = ListWrapper.monadCombine
    implicit val iso = MonoidalTests.Isomorphisms.covariant[OneAnd[?, ListWrapper]]
    checkAll("OneAnd[Int, ListWrapper]", MonoidalTests[OneAnd[?, ListWrapper]].monoidal[Int, Int, Int])
    checkAll("Monoidal[OneAnd[A, ListWrapper]]", SerializableTests.serializable(Monoidal[OneAnd[?, ListWrapper]]))
  }

  {
    implicit val functor = ListWrapper.functor
    checkAll("OneAnd[Int, ListWrapper]", FunctorTests[OneAnd[?, ListWrapper]].functor[Int, Int, Int])
    checkAll("Functor[OneAnd[A, ListWrapper]]", SerializableTests.serializable(Functor[OneAnd[?, ListWrapper]]))
  }

  {
    implicit val monadCombine = ListWrapper.monadCombine
    checkAll("OneAnd[Int, ListWrapper]", SemigroupKTests[OneAnd[?, ListWrapper]].semigroupK[Int])
    checkAll("OneAnd[Int, List]", GroupLaws[OneAnd[Int, List]].semigroup)
    checkAll("SemigroupK[OneAnd[A, ListWrapper]]", SerializableTests.serializable(SemigroupK[OneAnd[?, ListWrapper]]))
    checkAll("Semigroup[NonEmptyList[Int]]", SerializableTests.serializable(Semigroup[OneAnd[Int, List]]))
  }

  {
    implicit val foldable = ListWrapper.foldable
    checkAll("OneAnd[Int, ListWrapper]", FoldableTests[OneAnd[?, ListWrapper]].foldable[Int, Int])
    checkAll("Foldable[OneAnd[A, ListWrapper]]", SerializableTests.serializable(Foldable[OneAnd[?, ListWrapper]]))
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
