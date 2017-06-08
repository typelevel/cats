package cats
package tests

import cats.Functor
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
    PropertyCheckConfiguration(minSuccessful = 20, sizeRange = 5)

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
    // FunctorFilter composition
    implicit val instance = ListWrapper.functorFilter
    checkAll("Nested[List, ListWrapper, ?]", FunctorFilterTests[Nested[List, ListWrapper, ?]].functorFilter[Int, Int, Int])
    checkAll("FunctorFilter[Nested[List, ListWrapper, ?]]", SerializableTests.serializable(FunctorFilter[Nested[List, ListWrapper, ?]]))

    test("collect consistency") {
      forAll { l: Nested[List, ListWrapper, Int] =>
        l.collect(evenPf).value should === (l.value.map(_.collect(evenPf)))
      }
    }

    test("filter consistency") {
      forAll { l: Nested[List, ListWrapper, Int] =>
        l.filter(even).value should === (l.value.map(_.filter(even)))
      }
    }
  }

  {
    // Covariant + contravariant functor composition
    checkAll("Nested[Option, Show, ?]", ContravariantTests[Nested[Option, Show, ?]].contravariant[Int, Int, Int])
    checkAll("Contravariant[Nested[Option, Show, ?]]", SerializableTests.serializable(Contravariant[Nested[Option, Show, ?]]))
  }

  {
    // Contravariant + Contravariant = Functor
    type ConstInt[A] = Const[Int, A]
    checkAll("Nested[Const[Int, ?], Show, ?]", FunctorTests[Nested[ConstInt, Show, ?]].functor[Int, Int, Int])
    checkAll("Functor[Nested[Const[Int, ?], Show, ?]]", SerializableTests.serializable(Functor[Nested[ConstInt, Show, ?]]))
  }

  {
    // Contravariant + Functor = Contravariant
    checkAll("Nested[Show, Option, ?]", ContravariantTests[Nested[Show, Option, ?]].contravariant[Int, Int, Int])
    checkAll("Contravariant[Nested[Show, Option, ?]]", SerializableTests.serializable(Contravariant[Nested[Show, Option, ?]]))
  }

  {
    // Apply composition
    implicit val instance = ListWrapper.applyInstance
    checkAll("Nested[List, ListWrapper, ?]", ApplyTests[Nested[List, ListWrapper, ?]].apply[Int, Int, Int])
    checkAll("Apply[Nested[List, ListWrapper, ?]]", SerializableTests.serializable(Apply[Nested[List, ListWrapper, ?]]))
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
    // Traverse composition
    implicit val instance = ListWrapper.traverse
    checkAll("Nested[List, ListWrapper, ?]", TraverseTests[Nested[List, ListWrapper, ?]].traverse[Int, Int, Int, List[Int], Option, Option])
    checkAll("Traverse[Nested[List, ListWrapper, ?]]", SerializableTests.serializable(Traverse[Nested[List, ListWrapper, ?]]))
  }

  {
    // TraverseFilter composition
    implicit val instance = ListWrapper.traverseFilter
    checkAll("Nested[List, ListWrapper, ?]", TraverseFilterTests[Nested[List, ListWrapper, ?]].traverseFilter[Int, Int, Int, List[Int], Option, Option])
    checkAll("TraverseFilter[Nested[List, ListWrapper, ?]]", SerializableTests.serializable(TraverseFilter[Nested[List, ListWrapper, ?]]))
  }

  {
    implicit val foldable = ListWrapper.foldable
    checkAll("Nested[NonEmptyList, OneAnd[ListWrapper, ?], ?]", ReducibleTests[Nested[NonEmptyList, OneAnd[ListWrapper, ?], ?]].reducible[Option, Int, Int])
    checkAll("Reducible[Nested[NonEmptyList, OneAnd[ListWrapper, ?], ?]]", SerializableTests.serializable(Reducible[Nested[NonEmptyList, OneAnd[ListWrapper, ?], ?]]))
  }

  {
    //NonEmptyTraverse composition
    checkAll("Nested[NonEmptyList, NonEmptyVector, ?]", NonEmptyTraverseTests[Nested[NonEmptyList, NonEmptyVector, ?]].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option])
    checkAll("NonEmptyTraverse[Nested[NonEmptyList, NonEmptyVector, ?]]", SerializableTests.serializable(NonEmptyTraverse[Nested[NonEmptyList, NonEmptyVector, ?]]))

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
