package cats
package tests

import algebra.laws.OrderLaws

import cats.data.{NonEmptyList, OneAnd}
import cats.laws.discipline.{ComonadTests, FunctorTests, SemigroupKTests, FoldableTests, MonadTests, SerializableTests}
import cats.laws.discipline.arbitrary.{evalArbitrary, oneAndArbitrary}

import org.scalacheck.Prop._

import scala.util.Random

class OneAndTests extends CatsSuite {
  checkAll("OneAnd[Int, List]", OrderLaws[OneAnd[Int, List]].eqv)

  // Test instances that have more general constraints
  {
    implicit val functor = ListWrapper.functor
    checkAll("OneAnd[Int, ListWrapper]", FunctorTests[OneAnd[?, ListWrapper]].functor[Int, Int, Int])
    checkAll("Functor[OneAnd[A, ListWrapper]]", SerializableTests.serializable(Functor[OneAnd[?, ListWrapper]]))
  }

  {
    implicit val monadCombine = ListWrapper.monadCombine
    checkAll("OneAnd[Int, ListWrapper]", SemigroupKTests[OneAnd[?, ListWrapper]].semigroupK[Int])
    checkAll("SemigroupK[OneAnd[A, ListWrapper]]", SerializableTests.serializable(SemigroupK[OneAnd[?, ListWrapper]]))
  }

  {
    implicit val foldable = ListWrapper.foldable
    checkAll("OneAnd[Int, ListWrapper]", FoldableTests[OneAnd[?, ListWrapper]].foldable[Int, Int])
    checkAll("Foldable[OneAnd[A, ListWrapper]]", SerializableTests.serializable(Foldable[OneAnd[?, ListWrapper]]))
  }

  checkAll("NonEmptyList[Int]", MonadTests[NonEmptyList].monad[Int, Int, Int])
  checkAll("Monad[NonEmptyList[A]]", SerializableTests.serializable(Monad[NonEmptyList]))

  checkAll("NonEmptyList[Int]", ComonadTests[NonEmptyList].comonad[Int, Int, Int])
  checkAll("Comonad[NonEmptyList[A]]", SerializableTests.serializable(Comonad[NonEmptyList]))

  test("Creating OneAnd + unwrap is identity")(check {
    forAll { (list: List[Int]) => (list.size >= 1) ==> {
      val oneAnd = NonEmptyList(list.head, list.tail: _*)
      list == oneAnd.unwrap
    }}
  })

  test("NonEmptyList#filter is consistent with List#filter")(check {
    forAll { (nel: NonEmptyList[Int], p: Int => Boolean) =>
      val list = nel.unwrap
      nel.filter(p) == list.filter(p)
    }
  })

  test("NonEmptyList#find is consistent with List#find")(check {
    forAll { (nel: NonEmptyList[Int], p: Int => Boolean) =>
      val list = nel.unwrap
      nel.find(p) == list.find(p)
    }
  })

  test("NonEmptyList#exists is consistent with List#exists")(check {
    forAll { (nel: NonEmptyList[Int], p: Int => Boolean) =>
      val list = nel.unwrap
      nel.exists(p) == list.exists(p)
    }
  })

  test("NonEmptyList#forall is consistent with List#forall")(check {
    forAll { (nel: NonEmptyList[Int], p: Int => Boolean) =>
      val list = nel.unwrap
      nel.forall(p) == list.forall(p)
    }
  })
}
