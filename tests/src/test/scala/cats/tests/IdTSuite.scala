package cats
package tests

import cats.data.{Const, IdT, NonEmptyList}
import cats.kernel.laws.discipline.{OrderTests, EqTests}
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import Helpers.CSemi

class IdTSuite extends CatsSuite {

  implicit val iso = SemigroupalTests.Isomorphisms.invariant[IdT[ListWrapper, ?]](IdT.catsDataFunctorForIdT(ListWrapper.functor))

  checkAll("IdT[(CSemi, ?), Int]", CommutativeFlatMapTests[IdT[(CSemi, ?), ?]].commutativeFlatMap[Int, Int, Int])
  checkAll("CommutativeFlatMap[IdT[(CSemi, ?), ?]]", SerializableTests.serializable(CommutativeFlatMap[IdT[(CSemi, ?), ?]]))

  checkAll("IdT[Option, Int]", CommutativeMonadTests[IdT[Option, ?]].commutativeMonad[Int, Int, Int])
  checkAll("CommutativeMonad[IdT[Option, ?]]", SerializableTests.serializable(CommutativeMonad[IdT[Option, ?]]))

  {
    implicit val F = ListWrapper.eqv[Option[Int]]

    checkAll("IdT[ListWrapper, Int]", EqTests[IdT[ListWrapper, Int]].eqv)
    checkAll("Eq[IdT[ListWrapper, Int]]", SerializableTests.serializable(Eq[IdT[ListWrapper, Int]]))
  }

  {
    implicit val F = ListWrapper.order[Int]

    checkAll("IdT[ListWrapper, Int]", OrderTests[IdT[ListWrapper, Int]].order)
    checkAll("Order[IdT[ListWrapper, Int]]", SerializableTests.serializable(Order[IdT[ListWrapper, Int]]))
  }

  {
    implicit val F = ListWrapper.functor

    checkAll("IdT[ListWrapper, Int]", FunctorTests[IdT[ListWrapper, ?]].functor[Int, Int, Int])
    checkAll("Functor[IdT[ListWrapper, ?]]", SerializableTests.serializable(Functor[IdT[ListWrapper, ?]]))
  }

  {
    implicit val F = ListWrapper.applyInstance

    checkAll("IdT[ListWrapper, Int]", ApplyTests[IdT[ListWrapper, ?]].apply[Int, Int, Int])
    checkAll("Apply[IdT[ListWrapper, ?]]", SerializableTests.serializable(Apply[IdT[ListWrapper, ?]]))
  }

  {
    implicit val F = ListWrapper.applicative

    checkAll("IdT[ListWrapper, Int]", ApplicativeTests[IdT[ListWrapper, ?]].applicative[Int, Int, Int])
    checkAll("Applicative[IdT[ListWrapper, ?]]", SerializableTests.serializable(Applicative[IdT[ListWrapper, ?]]))
  }

  {
    checkAll("IdT[Const[String, ?], ?]",
      ContravariantMonoidalTests[IdT[Const[String, ?], ?]].contravariantMonoidal[Int, Int, Int])
    checkAll("ContravariantMonoidal[IdT[Const[String, ?], ?]]",
      SerializableTests.serializable(ContravariantMonoidal[IdT[Const[String, ?], ?]]))
  }

  {
    implicit val F = ListWrapper.flatMap

    checkAll("IdT[ListWrapper, Int]", FlatMapTests[IdT[ListWrapper, ?]].flatMap[Int, Int, Int])
    checkAll("FlatMap[IdT[ListWrapper, ?]]", SerializableTests.serializable(FlatMap[IdT[ListWrapper, ?]]))
  }

  {
    implicit val F = ListWrapper.monad

    checkAll("IdT[ListWrapper, Int]", MonadTests[IdT[ListWrapper, ?]].monad[Int, Int, Int])
    checkAll("Monad[IdT[ListWrapper, ?]]", SerializableTests.serializable(Monad[IdT[ListWrapper, ?]]))
  }

  {
    implicit val F = ListWrapper.foldable

    checkAll("IdT[ListWrapper, Int]", FoldableTests[IdT[ListWrapper, ?]].foldable[Int, Int])
    checkAll("Foldable[IdT[ListWrapper, ?]]", SerializableTests.serializable(Foldable[IdT[ListWrapper, ?]]))
  }

  {
    implicit val F = ListWrapper.traverse

    checkAll("IdT[ListWrapper, Int] with Option", TraverseTests[IdT[ListWrapper, ?]].traverse[Int, Int, Int, Int, Option, Option])
    checkAll("Traverse[IdT[ListWrapper, ?]]", SerializableTests.serializable(Traverse[IdT[ListWrapper, ?]]))
  }

  {
    implicit val F = NonEmptyList.catsDataInstancesForNonEmptyList

    checkAll("IdT[NonEmptyList, Int]", NonEmptyTraverseTests[IdT[NonEmptyList, ?]].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option])
    checkAll("NonEmptyTraverse[IdT[NonEmptyList, ?]]", SerializableTests.serializable(NonEmptyTraverse[IdT[NonEmptyList, ?]]))
  }


  test("flatMap and flatMapF consistent") {
    forAll { (idT: IdT[Option, Int], f: Int => IdT[Option, Int])  =>
      idT.flatMap(f) should === (idT.flatMapF(f(_).value))
    }
  }

  test("mapK consistent with f(value)+pure") {
    val f: List ~> Option = λ[List ~> Option](_.headOption)
    forAll { (idT: IdT[List, Int]) =>
      idT.mapK(f) should === (IdT(f(idT.value)))
    }
  }

}
