/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.tests

import cats.*
import cats.data.*
import cats.laws.discipline.*
import cats.laws.discipline.SemigroupalTests.Isomorphisms.*
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import org.scalacheck.Test.Parameters
import org.scalacheck.Arbitrary

class NestedSuite extends CatsSuite {
  // we have a lot of generated lists of lists in these tests. We have to tell
  // ScalaCheck to calm down a bit so we don't hit memory and test duration
  // issues.
  implicit override val scalaCheckTestParameters: Parameters =
    Parameters.default.withMinSuccessfulTests(20).withMaxSize(Parameters.default.minSize + 5)

  checkAll("Nested[Eval, List, *]", DeferTests[Nested[Eval, List, *]].defer[Int])

  {
    // Invariant composition
    implicit val instance: Invariant[ListWrapper] = ListWrapper.invariant
    checkAll("Nested[ListWrapper, ListWrapper]",
             InvariantTests[Nested[ListWrapper, ListWrapper, *]].invariant[Int, Int, Int]
    )
    checkAll("Invariant[Nested[ListWrapper, ListWrapper, *]]",
             SerializableTests.serializable(Invariant[Nested[ListWrapper, ListWrapper, *]])
    )
  }

  {
    // FunctorFilter composition
    implicit val instance: FunctorFilter[ListWrapper] = ListWrapper.functorFilter
    implicit val functorInstance: Functor[ListWrapper] = ListWrapper.functor
    checkAll("Nested[ListWrapper, ListWrapper]",
             FunctorFilterTests[Nested[ListWrapper, ListWrapper, *]].functorFilter[Int, Int, Int]
    )
    checkAll("FunctorFilter[Nested[ListWrapper, ListWrapper, *]]",
             SerializableTests.serializable(FunctorFilter[Nested[ListWrapper, ListWrapper, *]])
    )
  }

  {
    // TraverseFilter composition
    implicit val instance: TraverseFilter[ListWrapper] = ListWrapper.traverseFilter
    implicit val traverseInstance: Traverse[ListWrapper] = ListWrapper.traverse
    checkAll("Nested[ListWrapper, ListWrapper]",
             TraverseFilterTests[Nested[ListWrapper, ListWrapper, *]].traverseFilter[Int, Int, Int]
    )
    checkAll("TraverseFilter[Nested[ListWrapper, ListWrapper, *]]",
             SerializableTests.serializable(TraverseFilter[Nested[ListWrapper, ListWrapper, *]])
    )
  }

  {
    // Invariant + Covariant = Invariant
    val instance: Invariant[Nested[ListWrapper, ListWrapper, *]] =
      Nested.catsDataInvariantForCovariantNested(using ListWrapper.invariant, ListWrapper.functor)
    checkAll("Nested[ListWrapper, ListWrapper] - Invariant + Covariant",
             InvariantTests[Nested[ListWrapper, ListWrapper, *]](using instance).invariant[Int, Int, Int]
    )
    checkAll("Invariant[Nested[ListWrapper, ListWrapper, *]] - Invariant + Covariant",
             SerializableTests.serializable(instance)
    )
  }

  {
    // Invariant + Contravariant = Invariant
    val instance = Nested.catsDataInvariantForNestedContravariant(using ListWrapper.invariant, Contravariant[Show])
    checkAll("Nested[ListWrapper, Show, *]",
             InvariantTests[Nested[ListWrapper, Show, *]](using instance).invariant[MiniInt, Int, Boolean]
    )
    checkAll("Invariant[Nested[ListWrapper, Show, *]]", SerializableTests.serializable(instance))
  }

  {
    // Functor composition
    implicit val instance: Functor[ListWrapper] = ListWrapper.functor
    checkAll("Nested[Option, ListWrapper, *]", FunctorTests[Nested[Option, ListWrapper, *]].functor[Int, Int, Int])
    checkAll("Functor[Nested[Option, ListWrapper, *]]",
             SerializableTests.serializable(Functor[Nested[Option, ListWrapper, *]])
    )
  }

  {
    // Covariant + contravariant functor composition
    checkAll("Nested[Option, Show, *]",
             ContravariantTests[Nested[Option, Show, *]].contravariant[MiniInt, Int, Boolean]
    )
    checkAll("Contravariant[Nested[Option, Show, *]]",
             SerializableTests.serializable(Contravariant[Nested[Option, Show, *]])
    )
  }

  {
    // InvariantSemigroupal + Apply functor composition
    implicit val instance: InvariantSemigroupal[ListWrapper] = ListWrapper.invariantSemigroupal
    checkAll("Nested[ListWrapper, Option, *]",
             InvariantSemigroupalTests[Nested[ListWrapper, Option, *]].invariantSemigroupal[Int, Int, Int]
    )
    checkAll("InvariantSemigroupal[Nested[ListWrapper, Const[String, *], *]",
             SerializableTests.serializable(InvariantSemigroupal[Nested[ListWrapper, Option, *]])
    )
  }

  {
    // Applicative + ContravariantMonoidal functor composition
    checkAll("Nested[Option, Const[String, *], *]",
             ContravariantMonoidalTests[Nested[Option, Const[String, *], *]].contravariantMonoidal[Int, Int, Int]
    )
    checkAll("ContravariantMonoidal[Nested[Option, Const[String, *], *]",
             SerializableTests.serializable(ContravariantMonoidal[Nested[Option, Const[String, *], *]])
    )
  }

  {
    // Contravariant + Contravariant = Functor
    type ConstInt[A] = Const[Int, A]
    checkAll("Nested[Const[Int, *], Show, *]", FunctorTests[Nested[ConstInt, Show, *]].functor[Int, Int, Int])
    checkAll("Functor[Nested[Const[Int, *], Show, *]]",
             SerializableTests.serializable(Functor[Nested[ConstInt, Show, *]])
    )
  }

  {
    // Contravariant + Functor = Contravariant
    checkAll("Nested[Show, Option, *]",
             ContravariantTests[Nested[Show, Option, *]].contravariant[MiniInt, Int, Boolean]
    )
    checkAll("Contravariant[Nested[Show, Option, *]]",
             SerializableTests.serializable(Contravariant[Nested[Show, Option, *]])
    )
  }

  {
    // Apply composition
    implicit val instance: Apply[ListWrapper] = ListWrapper.applyInstance
    checkAll("Nested[List, ListWrapper, *]", ApplyTests[Nested[List, ListWrapper, *]].apply[Int, Int, Int])
    checkAll("Apply[Nested[List, ListWrapper, *]]", SerializableTests.serializable(Apply[Nested[List, ListWrapper, *]]))
  }

  {
    // CommutativeApply composition
    checkAll("Nested[Option, Validated[Int, *], *]",
             CommutativeApplyTests[Nested[Option, Validated[Int, *], *]].commutativeApply[Int, Int, Int]
    )
    checkAll("CommutativeApply[Nested[Option, Validated[Int, *], *], *]]",
             SerializableTests.serializable(CommutativeApply[Nested[Option, Validated[Int, *], *]])
    )
  }

  {
    // Applicative composition
    implicit val instance: Applicative[ListWrapper] = ListWrapper.applicative
    checkAll("Nested[List, ListWrapper, *]", ApplicativeTests[Nested[List, ListWrapper, *]].applicative[Int, Int, Int])
    checkAll("Applicative[Nested[List, ListWrapper, *]]",
             SerializableTests.serializable(Applicative[Nested[List, ListWrapper, *]])
    )
  }

  {
    // CommutativeApplicative composition
    implicit val instance: Applicative[ListWrapper] = ListWrapper.applicative
    checkAll("Nested[Option, Validated[Int, *], *]",
             CommutativeApplicativeTests[Nested[Option, Validated[Int, *], *]].commutativeApplicative[Int, Int, Int]
    )
    checkAll("CommutativeApplicative[Nested[List, ListWrapper, *]]",
             SerializableTests.serializable(CommutativeApplicative[Nested[Option, Validated[Int, *], *]])
    )
  }

  {
    // ApplicativeError composition
    implicit val instance: Applicative[ListWrapper] = ListWrapper.applicative

    checkAll(
      "Nested[Validated[String, *], ListWrapper, *]",
      ApplicativeErrorTests[Nested[Validated[String, *], ListWrapper, *], String].applicativeError[Int, Int, Int]
    )
    checkAll(
      "ApplicativeError[Nested[Validated[String, *], ListWrapper, *]]",
      SerializableTests.serializable(ApplicativeError[Nested[Validated[String, *], ListWrapper, *], String])
    )
  }

  {
    // Alternative composition
    implicit val instance: Alternative[ListWrapper] = ListWrapper.alternative
    checkAll("Nested[List, ListWrapper, *]", AlternativeTests[Nested[List, ListWrapper, *]].alternative[Int, Int, Int])
    checkAll("Alternative[Nested[List, ListWrapper, *]]",
             SerializableTests.serializable(Alternative[Nested[List, ListWrapper, *]])
    )
  }

  {
    // Foldable composition
    implicit val instance: Foldable[ListWrapper] = ListWrapper.foldable
    checkAll("Nested[List, ListWrapper, *]", FoldableTests[Nested[List, ListWrapper, *]].foldable[Int, Int])
    checkAll("Foldable[Nested[List, ListWrapper, *]]",
             SerializableTests.serializable(Foldable[Nested[List, ListWrapper, *]])
    )
  }

  {
    // Traverse composition
    implicit val instance: Traverse[ListWrapper] = ListWrapper.traverse
    checkAll("Nested[List, ListWrapper, *]",
             TraverseTests[Nested[List, ListWrapper, *]].traverse[Int, Int, Int, Set[Int], Option, Option]
    )
    checkAll("Traverse[Nested[List, ListWrapper, *]]",
             SerializableTests.serializable(Traverse[Nested[List, ListWrapper, *]])
    )
  }

  {
    // Reducible composition
    implicit val instance: Foldable[ListWrapper] = ListWrapper.foldable
    checkAll("Nested[NonEmptyList, OneAnd[ListWrapper, *], *]",
             ReducibleTests[Nested[NonEmptyList, OneAnd[ListWrapper, *], *]].reducible[Option, Int, Int]
    )
    checkAll(
      "Reducible[Nested[NonEmptyList, OneAnd[ListWrapper, *], *]]",
      SerializableTests.serializable(Reducible[Nested[NonEmptyList, OneAnd[ListWrapper, *], *]])
    )
  }

  {
    // NonEmptyTraverse composition
    checkAll(
      "Nested[NonEmptyList, NonEmptyVector, *]",
      NonEmptyTraverseTests[Nested[NonEmptyList, NonEmptyVector, *]]
        .nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
    )
    checkAll("NonEmptyTraverse[Nested[NonEmptyList, NonEmptyVector, *]]",
             SerializableTests.serializable(NonEmptyTraverse[Nested[NonEmptyList, NonEmptyVector, *]])
    )
  }

  {
    // SemigroupK composition
    implicit val instance: SemigroupK[ListWrapper] = ListWrapper.semigroupK
    checkAll("Nested[ListWrapper, Option, *]", SemigroupKTests[Nested[ListWrapper, Option, *]].semigroupK[Int])
    checkAll("SemigroupK[Nested[ListWrapper, Option, *]]",
             SerializableTests.serializable(SemigroupK[Nested[ListWrapper, Option, *]])
    )
  }

  {
    // MonoidK composition
    implicit val instance: MonoidK[ListWrapper] = ListWrapper.monoidK
    checkAll("Nested[ListWrapper, Option, *]", MonoidKTests[Nested[ListWrapper, Option, *]].monoidK[Int])
    checkAll("MonoidK[Nested[ListWrapper, Option, *]]",
             SerializableTests.serializable(MonoidK[Nested[ListWrapper, Option, *]])
    )
  }

  {
    import cats.laws.discipline.eq.*
    // Distributive composition
    checkAll(
      "Nested[Function1[MiniInt, *], Function0, *]",
      DistributiveTests[Nested[Function1[MiniInt, *], Function0, *]].distributive[Int, Int, Int, Option, Function0]
    )
    checkAll("Distributive[Nested[Function1[Int,*], Function0, *]]",
             SerializableTests.serializable(Distributive[Nested[Function1[Int, *], Function0, *]])
    )
  }

  {
    type Pair[A] = (A, A)

    // Scala 2.12 implicit resolution absolutely loses its mind here
    implicit val help_scala2_12: Representable.Aux[Nested[Pair, Pair, *], (Boolean, Boolean)] =
      Nested.catsDataRepresentableForNested[Pair, Pair]

    val a: Arbitrary[Int] = implicitly[Arbitrary[Int]]
    val b: Arbitrary[Nested[Pair, Pair, Int]] = implicitly[Arbitrary[Nested[Pair, Pair, Int]]]
    val c: Arbitrary[(Boolean, Boolean)] = implicitly[Arbitrary[(Boolean, Boolean)]]
    val d: Arbitrary[((Boolean, Boolean)) => Int] = implicitly[Arbitrary[((Boolean, Boolean)) => Int]]
    val e: Eq[Nested[Pair, Pair, Int]] = Eq[Nested[Pair, Pair, Int]]
    val f: Eq[Int] = Eq[Int]

    checkAll(
      "Nested[Pair, Pair, *]",
      RepresentableTests[Nested[Pair, Pair, *], (Boolean, Boolean)].representable[Int](
        a,
        b,
        c,
        d,
        e,
        f
      )
    )
    checkAll("Representable[Nested[Pair, Pair, *]]",
             SerializableTests.serializable(Representable[Nested[Pair, Pair, *]])
    )
  }

  {
    // Align composition
    checkAll(
      "Nested[List, Option, *]",
      AlignTests[Nested[List, Option, *]].align[Int, Int, Int, Int]
    )
  }
}
