package cats
package tests

import cats.data._
import cats.functor.Contravariant
import cats.laws.discipline._
import cats.laws.discipline.CartesianTests.Isomorphisms._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq.showEq

class NestedTests extends CatsSuite {
  // we have a lot of generated lists of lists in these tests. We have to tell
  // Scalacheck to calm down a bit so we don't hit memory and test duration
  // issues.
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(maxSize = 5, minSuccessful = 20)

  implicit val iso = {
    implicit val instance = ListWrapper.functor
    invariant[Nested[List, ListWrapper, ?]]
  }

  {
    // Functor composition
    implicit val instance = ListWrapper.functor
    checkAll("Nested[Option, ListWrapper, ?]", FunctorTests[Nested[Option, ListWrapper, ?]].functor[Int, Int, Int])
    checkAll("Functor[Nested[Option, ListWrapper, ?]]", SerializableTests.serializable(Functor[Nested[Option, ListWrapper, ?]]))
  }

  {
    // Covariant + contravariant functor composition
    checkAll("Nested[Option, Show, ?]", ContravariantTests[Nested[Option, Show, ?]].contravariant[Int, Int, Int])
    checkAll("Contravariant[Nested[Option, Show, ?]]", SerializableTests.serializable(Contravariant[Nested[Option, Show, ?]]))
  }

  {
    // Applicative composition
    implicit val instance = ListWrapper.applicative
    checkAll("Nested[List, ListWrapper, ?]", ApplicativeTests[Nested[List, ListWrapper, ?]].applicative[Int, Int, Int])
    checkAll("Applicative[Nested[List, ListWrapper, ?]]", SerializableTests.serializable(Applicative[Nested[List, ListWrapper, ?]]))
  }

  {
    // Alternative composition
    implicit val instance = ListWrapper.alternative
    checkAll("Nested[List, ListWrapper, ?]", AlternativeTests[Nested[List, ListWrapper, ?]].alternative[Int, Int, Int])
    checkAll("Alternative[Nested[List, ListWrapper, ?]]", SerializableTests.serializable(Alternative[Nested[List, ListWrapper, ?]]))
  }

  {
    // Foldable composition
    implicit val instance = ListWrapper.foldable
    checkAll("Nested[List, ListWrapper, ?]", FoldableTests[Nested[List, ListWrapper, ?]].foldable[Int, Int])
    checkAll("Foldable[Nested[List, ListWrapper, ?]]", SerializableTests.serializable(Foldable[Nested[List, ListWrapper, ?]]))
  }

  {
    // SI-2712? It can resolve Reducible[NonEmptyList] and Reducible[NonEmptyVector] but not
    // Reducible[Nested[NonEmptyList, NonEmptyVector, ?]]
    // Similarly for Arbitrary.
    implicit val reducible = Nested.nestedReducible[NonEmptyList, NonEmptyVector]
    implicit val arbitrary0 = nestedArbitrary[NonEmptyList, NonEmptyVector, Int]
    implicit val arbitrary1 = nestedArbitrary[NonEmptyList, NonEmptyVector, Option[Int]]

    checkAll("Nested[NonEmptyList, NonEmptyVector, ?]", ReducibleTests[Nested[NonEmptyList, NonEmptyVector, ?]].reducible[Option, Int, Int])
    checkAll("Reducible[Nested[NonEmptyList, NonEmptyVector, ?]]", SerializableTests.serializable(reducible))
  }

  {
    // SemigroupK composition
    implicit val instance = ListWrapper.semigroupK
    checkAll("Nested[List, ListWrapper, ?]", SemigroupKTests[Nested[List, ListWrapper, ?]].semigroupK[Int])
    checkAll("SemigroupK[Nested[List, ListWrapper, ?]]", SerializableTests.serializable(SemigroupK[Nested[List, ListWrapper, ?]]))
  }

  {
    // MonoidK composition
    implicit val instance = ListWrapper.monoidK
    checkAll("Nested[List, ListWrapper, ?]", MonoidKTests[Nested[List, ListWrapper, ?]].monoidK[Int])
    checkAll("MonoidK[Nested[List, ListWrapper, ?]]", SerializableTests.serializable(MonoidK[Nested[List, ListWrapper, ?]]))
  }
}
