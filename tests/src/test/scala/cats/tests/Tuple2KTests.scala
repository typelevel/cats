package cats
package tests

import cats.data.Tuple2K
import cats.functor.Contravariant
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import cats.kernel.laws.OrderLaws

class Tuple2KTests extends CatsSuite {
  implicit val iso = CartesianTests.Isomorphisms.invariant[Tuple2K[Option, List, ?]]
  checkAll("Tuple2K[Option, List, Int]", CartesianTests[λ[α => Tuple2K[Option, List, α]]].cartesian[Int, Int, Int])
  checkAll("Cartesian[Tuple2K[Option, List, Int]]", SerializableTests.serializable(Cartesian[λ[α => Tuple2K[Option, List, α]]]))

  checkAll("Tuple2K[Option, List, Int]", AlternativeTests[λ[α => Tuple2K[Option, List, α]]].alternative[Int, Int, Int])
  checkAll("Alternative[Tuple2K[Option, List, Int]]", SerializableTests.serializable(Alternative[λ[α => Tuple2K[Option, List, α]]]))

  checkAll("Tuple2K[Show, Order, Int]", ContravariantTests[λ[α => Tuple2K[Show, Order, α]]].contravariant[Int, Int, Int])
  checkAll("Contravariant[Tuple2K[Show, Order, Int]]", SerializableTests.serializable(Contravariant[λ[α => Tuple2K[Show, Order, α]]]))

  checkAll("Show[Tuple2K[Option, Option, Int]]", SerializableTests.serializable(Show[Tuple2K[Option, Option, Int]]))

  {
    implicit val monoidK = ListWrapper.monoidK
    checkAll("Tuple2K[ListWrapper, ListWrapper, ?]", MonoidKTests[Tuple2K[ListWrapper, ListWrapper, ?]].monoidK[Int])
    checkAll("MonoidK[Tuple2K[ListWrapper, ListWrapper, ?]]", SerializableTests.serializable(MonoidK[Tuple2K[ListWrapper, ListWrapper, ?]]))
  }

  {
    implicit val semigroupK = ListWrapper.semigroupK
    checkAll("Tuple2K[ListWrapper, ListWrapper, ?]", SemigroupKTests[Tuple2K[ListWrapper, ListWrapper, ?]].semigroupK[Int])
    checkAll("SemigroupK[Tuple2K[ListWrapper, ListWrapper, ?]]", SerializableTests.serializable(SemigroupK[Tuple2K[ListWrapper, ListWrapper, ?]]))
  }

  {
    implicit val apply = ListWrapper.applyInstance
    implicit val iso = CartesianTests.Isomorphisms.invariant[Tuple2K[ListWrapper, ListWrapper, ?]]
    checkAll("Tuple2K[ListWrapper, ListWrapper, ?]", ApplyTests[Tuple2K[ListWrapper, ListWrapper, ?]].apply[Int, Int, Int])
    checkAll("Apply[Tuple2K[ListWrapper, ListWrapper, ?]]", SerializableTests.serializable(Apply[Tuple2K[ListWrapper, ListWrapper, ?]]))
  }

  {
    implicit val functor = ListWrapper.functor
    checkAll("Tuple2K[ListWrapper, ListWrapper, ?]", FunctorTests[Tuple2K[ListWrapper, ListWrapper, ?]].functor[Int, Int, Int])
    checkAll("Functor[Tuple2K[ListWrapper, ListWrapper, ?]]", SerializableTests.serializable(Functor[Tuple2K[ListWrapper, ListWrapper, ?]]))
  }

  {
    implicit val monad = ListWrapper.monad
    implicit val iso = CartesianTests.Isomorphisms.invariant[Tuple2K[ListWrapper, ListWrapper, ?]]
    checkAll("Tuple2K[ListWrapper, ListWrapper, ?]", MonadTests[Tuple2K[ListWrapper, ListWrapper, ?]].monad[Int, Int, Int])
    checkAll("Monad[Tuple2K[ListWrapper, ListWrapper, ?]]", SerializableTests.serializable(Monad[Tuple2K[ListWrapper, ListWrapper, ?]]))
  }

  {
    implicit val foldable = ListWrapper.foldable
    checkAll("Tuple2K[ListWrapper, ListWrapper, ?]", FoldableTests[Tuple2K[ListWrapper, ListWrapper, ?]].foldable[Int, Int])
    checkAll("Foldable[Tuple2K[ListWrapper, ListWrapper, ?]]", SerializableTests.serializable(Foldable[Tuple2K[ListWrapper, ListWrapper, ?]]))
  }

  {
    implicit val traverse = ListWrapper.traverse
    checkAll("Tuple2K[ListWrapper, ListWrapper, ?]", TraverseTests[Tuple2K[ListWrapper, ListWrapper, ?]].traverse[Int, Int, Int, Int, Option, Option])
    checkAll("Traverse[Tuple2K[ListWrapper, ListWrapper, ?]]", SerializableTests.serializable(Traverse[Tuple2K[ListWrapper, ListWrapper, ?]]))
  }

  {
    implicit val alternative = ListWrapper.alternative
    implicit val iso = CartesianTests.Isomorphisms.invariant[Tuple2K[ListWrapper, ListWrapper, ?]]
    checkAll("Tuple2K[ListWrapper, ListWrapper, ?]", AlternativeTests[Tuple2K[ListWrapper, ListWrapper, ?]].alternative[Int, Int, Int])
    checkAll("Alternative[Tuple2K[ListWrapper, ListWrapper, ?]]", SerializableTests.serializable(Alternative[Tuple2K[ListWrapper, ListWrapper, ?]]))
  }

  {
    implicit val monadCombine = ListWrapper.monadCombine
    implicit val iso = CartesianTests.Isomorphisms.invariant[Tuple2K[ListWrapper, ListWrapper, ?]]
    checkAll("Tuple2K[ListWrapper, ListWrapper, ?]", MonadCombineTests[Tuple2K[ListWrapper, ListWrapper, ?]].monadCombine[Int, Int, Int])
    checkAll("MonadCombine[Tuple2K[ListWrapper, ListWrapper, ?]]", SerializableTests.serializable(MonadCombine[Tuple2K[ListWrapper, ListWrapper, ?]]))
  }

  {
    implicit val E = ListWrapper.eqv[Int]
    implicit val O = ListWrapper.order[Int]
    implicit val P = ListWrapper.partialOrder[Int]

    checkAll("Tuple2K[ListWrapper, ListWrapper, Int]", OrderLaws[Tuple2K[ListWrapper, ListWrapper, Int]].eqv)
    checkAll("Tuple2K[ListWrapper, ListWrapper, Int]", OrderLaws[Tuple2K[ListWrapper, ListWrapper, Int]].order)
    checkAll("Tuple2K[ListWrapper, ListWrapper, Int]", OrderLaws[Tuple2K[ListWrapper, ListWrapper, Int]].partialOrder)
  }

  test("show") {
    forAll { (l1: Option[Int], l2: Option[Int]) =>
      val tuple = Tuple2K(l1, l2)

      Show[Tuple2K[Option, Option, Int]].show(tuple) should === (s"Tuple2K(${Show[Option[Int]].show(l1)}, ${Show[Option[Int]].show(l2)})")
    }
  }

}
