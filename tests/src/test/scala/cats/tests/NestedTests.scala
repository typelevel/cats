package cats
package tests

import cats.data._
import cats.functor._
import cats.laws.discipline._
import cats.laws.discipline.CartesianTests.Isomorphisms._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq.catsLawsEqForShow

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
    // Invariant composition
    implicit val instance = ListWrapper.invariant
    checkAll("Nested[ListWrapper, ListWrapper]", InvariantTests[Nested[ListWrapper, ListWrapper, ?]].invariant[Int, Int, Int])
    checkAll("Invariant[Nested[ListWrapper, ListWrapper, ?]]", SerializableTests.serializable(Invariant[Nested[ListWrapper, ListWrapper, ?]]))
  }

  {
    // Invariant + Covariant = Invariant
    val instance = Nested.catsDataInvariantForCovariantNested(ListWrapper.invariant, ListWrapper.functor)
    checkAll("Nested[ListWrapper, ListWrapper] - Invariant + Covariant", InvariantTests[Nested[ListWrapper, ListWrapper, ?]](instance).invariant[Int, Int, Int])
    checkAll("Invariant[Nested[ListWrapper, ListWrapper, ?]] - Invariant + Covariant", SerializableTests.serializable(instance))
  }

  {
    // Invariant + Contravariant = Invariant
    val instance = Nested.catsDataInvariantForNestedContravariant(ListWrapper.invariant, Contravariant[Show])
    checkAll("Nested[ListWrapper, Show]", InvariantTests[Nested[ListWrapper, Show, ?]](instance).invariant[Int, Int, Int])
    checkAll("Invariant[Nested[ListWrapper, Show, ?]]", SerializableTests.serializable(instance))
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
    // Contravariant + Contravariant = Functor
    type ConstInt[A] = Const[Int, A]
    // SI-2712
    implicit val instance = Nested.catsDataContravariantForNested[ConstInt, Show]
    implicit val arbitrary = catsLawsArbitraryForNested[ConstInt, Show, Int]
    implicit val eqv = Nested.catsDataEqForNested[ConstInt, Show, Int]
    checkAll("Nested[Const[Int, ?], Show, ?]", FunctorTests[Nested[ConstInt, Show, ?]].functor[Int, Int, Int])
    checkAll("Functor[Nested[Const[Int, ?], Show, ?]]", SerializableTests.serializable(instance))
  }

  {
    // Contravariant + Functor = Contravariant
    checkAll("Nested[Show, Option, ?]", ContravariantTests[Nested[Show, Option, ?]].contravariant[Int, Int, Int])
    checkAll("Contravariant[Nested[Show, Option, ?]]", SerializableTests.serializable(Contravariant[Nested[Show, Option, ?]]))
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
    implicit val reducible = Nested.catsDataReducibleForNested[NonEmptyList, NonEmptyVector]
    implicit val arbitrary0 = catsLawsArbitraryForNested[NonEmptyList, NonEmptyVector, Int]
    implicit val arbitrary1 = catsLawsArbitraryForNested[NonEmptyList, NonEmptyVector, Option[Int]]

    checkAll("Nested[NonEmptyList, NonEmptyVector, ?]", ReducibleTests[Nested[NonEmptyList, NonEmptyVector, ?]].reducible[Option, Int, Int])
    checkAll("Reducible[Nested[NonEmptyList, NonEmptyVector, ?]]", SerializableTests.serializable(reducible))
  }

  {
    // SemigroupK composition
    implicit val instance = ListWrapper.semigroupK
    checkAll("Nested[ListWrapper, Option, ?]", SemigroupKTests[Nested[ListWrapper, Option, ?]].semigroupK[Int])
    checkAll("SemigroupK[Nested[ListWrapper, Option, ?]]", SerializableTests.serializable(SemigroupK[Nested[ListWrapper, Option, ?]]))
  }

  {
    // MonoidK composition
    implicit val instance = ListWrapper.monoidK
    checkAll("Nested[ListWrapper, Option, ?]", MonoidKTests[Nested[ListWrapper, Option, ?]].monoidK[Int])
    checkAll("MonoidK[Nested[ListWrapper, Option, ?]]", SerializableTests.serializable(MonoidK[Nested[ListWrapper, Option, ?]]))
  }
}
