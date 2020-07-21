package cats.tests

import cats._
import cats.data.{Const, IdT, NonEmptyList}
import cats.kernel.laws.discipline.{EqTests, OrderTests}
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.tests.Helpers.CSemi

class IdTSuite extends CatsSuite {

  implicit val iso: Isomorphisms[IdT[ListWrapper, *]] =
    Isomorphisms.invariant[IdT[ListWrapper, *]](IdT.catsDataFunctorForIdT(ListWrapper.functor))

  checkAll("IdT[(CSemi, *), Int]", CommutativeFlatMapTests[IdT[(CSemi, *), *]].commutativeFlatMap[Int, Int, Int])
  checkAll("CommutativeFlatMap[IdT[(CSemi, *), *]]",
           SerializableTests.serializable(CommutativeFlatMap[IdT[(CSemi, *), *]])
  )

  checkAll("IdT[Option, Int]", CommutativeMonadTests[IdT[Option, *]].commutativeMonad[Int, Int, Int])
  checkAll("CommutativeMonad[IdT[Option, *]]", SerializableTests.serializable(CommutativeMonad[IdT[Option, *]]))

  {
    implicit val F: Eq[ListWrapper[Option[Int]]] = ListWrapper.eqv[Option[Int]]

    checkAll("IdT[ListWrapper, Int]", EqTests[IdT[ListWrapper, Int]].eqv)
    checkAll("Eq[IdT[ListWrapper, Int]]", SerializableTests.serializable(Eq[IdT[ListWrapper, Int]]))
  }

  {
    implicit val F: Order[ListWrapper[Int]] = ListWrapper.order[Int]

    checkAll("IdT[ListWrapper, Int]", OrderTests[IdT[ListWrapper, Int]].order)
    checkAll("Order[IdT[ListWrapper, Int]]", SerializableTests.serializable(Order[IdT[ListWrapper, Int]]))
  }

  {
    implicit val F: Functor[ListWrapper] = ListWrapper.functor

    checkAll("IdT[ListWrapper, Int]", FunctorTests[IdT[ListWrapper, *]].functor[Int, Int, Int])
    checkAll("Functor[IdT[ListWrapper, *]]", SerializableTests.serializable(Functor[IdT[ListWrapper, *]]))
  }

  {
    implicit val F: Apply[ListWrapper] = ListWrapper.applyInstance

    checkAll("IdT[ListWrapper, Int]", ApplyTests[IdT[ListWrapper, *]].apply[Int, Int, Int])
    checkAll("Apply[IdT[ListWrapper, *]]", SerializableTests.serializable(Apply[IdT[ListWrapper, *]]))
  }

  {
    implicit val F: Applicative[ListWrapper] = ListWrapper.applicative

    checkAll("IdT[ListWrapper, Int]", ApplicativeTests[IdT[ListWrapper, *]].applicative[Int, Int, Int])
    checkAll("Applicative[IdT[ListWrapper, *]]", SerializableTests.serializable(Applicative[IdT[ListWrapper, *]]))
  }

  {
    checkAll("IdT[Const[String, *], *]",
             ContravariantMonoidalTests[IdT[Const[String, *], *]].contravariantMonoidal[Int, Int, Int]
    )
    checkAll("ContravariantMonoidal[IdT[Const[String, *], *]]",
             SerializableTests.serializable(ContravariantMonoidal[IdT[Const[String, *], *]])
    )
  }

  {
    implicit val coproductIso = DecidableTests.Isomorphisms.invariant[IdT[Const[String, ?], ?]]
    checkAll("IdT[Const[String, ?], ?]", DecidableTests[IdT[Const[String, ?], ?]].decidable[Int, Int, Int])
    checkAll("Decidable[IdT[Const[String, ?], ?]]",
             SerializableTests.serializable(Decidable[IdT[Const[String, ?], ?]])
    )
  }

  {
    implicit val F: FlatMap[ListWrapper] = ListWrapper.flatMap

    checkAll("IdT[ListWrapper, Int]", FlatMapTests[IdT[ListWrapper, *]].flatMap[Int, Int, Int])
    checkAll("FlatMap[IdT[ListWrapper, *]]", SerializableTests.serializable(FlatMap[IdT[ListWrapper, *]]))
  }

  {
    implicit val F: Monad[ListWrapper] = ListWrapper.monad

    checkAll("IdT[ListWrapper, Int]", MonadTests[IdT[ListWrapper, *]].monad[Int, Int, Int])
    checkAll("Monad[IdT[ListWrapper, *]]", SerializableTests.serializable(Monad[IdT[ListWrapper, *]]))
  }

  {
    implicit val F: Foldable[ListWrapper] = ListWrapper.foldable

    checkAll("IdT[ListWrapper, Int]", FoldableTests[IdT[ListWrapper, *]].foldable[Int, Int])
    checkAll("Foldable[IdT[ListWrapper, *]]", SerializableTests.serializable(Foldable[IdT[ListWrapper, *]]))
  }

  {
    implicit val F: Traverse[ListWrapper] = ListWrapper.traverse

    checkAll("IdT[ListWrapper, Int] with Option",
             TraverseTests[IdT[ListWrapper, *]].traverse[Int, Int, Int, Int, Option, Option]
    )
    checkAll("Traverse[IdT[ListWrapper, *]]", SerializableTests.serializable(Traverse[IdT[ListWrapper, *]]))
  }

  {
    implicit val F: Traverse[NonEmptyList] = NonEmptyList.catsDataInstancesForNonEmptyList

    checkAll("IdT[NonEmptyList, Int]",
             NonEmptyTraverseTests[IdT[NonEmptyList, *]].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )
    checkAll("NonEmptyTraverse[IdT[NonEmptyList, *]]",
             SerializableTests.serializable(NonEmptyTraverse[IdT[NonEmptyList, *]])
    )
  }

  test("flatMap and flatMapF consistent") {
    forAll { (idT: IdT[Option, Int], f: Int => IdT[Option, Int]) =>
      idT.flatMap(f) should ===(idT.flatMapF(f(_).value))
    }
  }

  test("mapK consistent with f(value)+pure") {
    val f: List ~> Option = new (List ~> Option) { def apply[A](a: List[A]): Option[A] = a.headOption }
    forAll { (idT: IdT[List, Int]) =>
      idT.mapK(f) should ===(IdT(f(idT.value)))
    }
  }

}
