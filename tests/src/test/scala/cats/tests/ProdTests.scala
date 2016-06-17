package cats
package tests

import cats.data.Prod
import cats.functor.Contravariant
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._

class ProdTests extends CatsSuite {
  implicit val iso = CartesianTests.Isomorphisms.invariant[Prod[Option, List, ?]]
  checkAll("Prod[Option, List, Int]", CartesianTests[λ[α => Prod[Option, List, α]]].cartesian[Int, Int, Int])
  checkAll("Cartesian[Prod[Option, List, Int]]", SerializableTests.serializable(Cartesian[λ[α => Prod[Option, List, α]]]))

  checkAll("Prod[Option, List, Int]", AlternativeTests[λ[α => Prod[Option, List, α]]].alternative[Int, Int, Int])
  checkAll("Alternative[Prod[Option, List, Int]]", SerializableTests.serializable(Alternative[λ[α => Prod[Option, List, α]]]))

  checkAll("Prod[Show, Order, Int]", ContravariantTests[λ[α => Prod[Show, Order, α]]].contravariant[Int, Int, Int])
  checkAll("Contravariant[Prod[Show, Order, Int]]", SerializableTests.serializable(Contravariant[λ[α => Prod[Show, Order, α]]]))

  {
    implicit val monoidK = ListWrapper.monoidK
    checkAll("Prod[ListWrapper, ListWrapper, ?]", MonoidKTests[Prod[ListWrapper, ListWrapper, ?]].monoidK[Int])
    checkAll("MonoidK[Prod[ListWrapper, ListWrapper, ?]]", SerializableTests.serializable(MonoidK[Prod[ListWrapper, ListWrapper, ?]]))
  }

  {
    implicit val semigroupK = ListWrapper.semigroupK
    checkAll("Prod[ListWrapper, ListWrapper, ?]", SemigroupKTests[Prod[ListWrapper, ListWrapper, ?]].semigroupK[Int])
    checkAll("SemigroupK[Prod[ListWrapper, ListWrapper, ?]]", SerializableTests.serializable(SemigroupK[Prod[ListWrapper, ListWrapper, ?]]))
  }

  {
    implicit val apply = ListWrapper.applyInstance
    implicit val iso = CartesianTests.Isomorphisms.invariant[Prod[ListWrapper, ListWrapper, ?]]
    checkAll("Prod[ListWrapper, ListWrapper, ?]", ApplyTests[Prod[ListWrapper, ListWrapper, ?]].apply[Int, Int, Int])
    checkAll("Apply[Prod[ListWrapper, ListWrapper, ?]]", SerializableTests.serializable(Apply[Prod[ListWrapper, ListWrapper, ?]]))
  }

  {
    implicit val functor = ListWrapper.functor
    checkAll("Prod[ListWrapper, ListWrapper, ?]", FunctorTests[Prod[ListWrapper, ListWrapper, ?]].functor[Int, Int, Int])
    checkAll("Functor[Prod[ListWrapper, ListWrapper, ?]]", SerializableTests.serializable(Functor[Prod[ListWrapper, ListWrapper, ?]]))
  }
}
