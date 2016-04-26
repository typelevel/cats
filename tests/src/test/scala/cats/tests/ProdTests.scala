package cats
package tests

import cats.data.Prod
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._

class ProdTests extends CatsSuite {
  implicit val iso = CartesianTests.Isomorphisms.invariant[Prod[Option, List, ?]]
  checkAll("Prod[Option, List, Int]", CartesianTests[Lambda[X => Prod[Option, List, X]]].cartesian[Int, Int, Int])
  checkAll("Cartesian[Prod[Option, List, Int]]", SerializableTests.serializable(Cartesian[Lambda[X => Prod[Option, List, X]]]))

  checkAll("Prod[Option, List, Int]", AlternativeTests[Lambda[X => Prod[Option, List, X]]].alternative[Int, Int, Int])
  checkAll("Alternative[Prod[Option, List, Int]]", SerializableTests.serializable(Alternative[Lambda[X => Prod[Option, List, X]]]))

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
