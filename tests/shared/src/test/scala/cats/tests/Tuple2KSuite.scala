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
import cats.data.{Const, Tuple2K, Validated}
import cats.kernel.laws.discipline.{EqTests, OrderTests, PartialOrderTests}
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import cats.syntax.eq.*
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.*

class Tuple2KSuite extends CatsSuite {
  implicit val iso: Isomorphisms[Tuple2K[Option, List, *]] = Isomorphisms.invariant[Tuple2K[Option, List, *]]
  checkAll("Tuple2K[Eval, Eval, *]", DeferTests[Tuple2K[Eval, Eval, *]].defer[Int])
  checkAll("Tuple2K[Option, List, Int]", SemigroupalTests[λ[α => Tuple2K[Option, List, α]]].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Tuple2K[Option, List, Int]]",
           SerializableTests.serializable(Semigroupal[λ[α => Tuple2K[Option, List, α]]])
  )

  checkAll("Tuple2K[Option, List, Int]", AlternativeTests[λ[α => Tuple2K[Option, List, α]]].alternative[Int, Int, Int])
  checkAll("Alternative[Tuple2K[Option, List, Int]]",
           SerializableTests.serializable(Alternative[λ[α => Tuple2K[Option, List, α]]])
  )

  checkAll("Tuple2K[Show, Order, MiniInt]",
           ContravariantTests[λ[α => Tuple2K[Show, Order, α]]].contravariant[MiniInt, Int, Boolean]
  )
  checkAll("Contravariant[Tuple2K[Show, Order, Int]]",
           SerializableTests.serializable(Contravariant[λ[α => Tuple2K[Show, Order, α]]])
  )

  checkAll(
    "Tuple2K[Const[String, *], Const[Int, *], Int]",
    ContravariantMonoidalTests[λ[α => Tuple2K[Const[String, *], Const[Int, *], α]]].contravariantMonoidal[Int, Int, Int]
  )
  checkAll(
    "ContravariantMonoidal[Tuple2K[Const[String, *], Const[Int, *], Int]]",
    SerializableTests.serializable(ContravariantMonoidal[λ[α => Tuple2K[Const[String, *], Const[Int, *], α]]])
  )

  checkAll("Show[Tuple2K[Option, Option, Int]]", SerializableTests.serializable(Show[Tuple2K[Option, Option, Int]]))

  {
    type Pair[A] = (A, A)

    // Scala 2.12 implicit resolution absolutely loses its mind here
    implicit val help_scala2_12: Representable.Aux[Tuple2K[Pair, Pair, *], Either[Boolean, Boolean]] =
      Tuple2K.catsDataRepresentableForTuple2K[Pair, Pair]

    val a: Arbitrary[Int] = implicitly[Arbitrary[Int]]
    val b: Arbitrary[Tuple2K[Pair, Pair, Int]] = implicitly[Arbitrary[Tuple2K[Pair, Pair, Int]]]
    val c: Arbitrary[Either[Boolean, Boolean]] = implicitly[Arbitrary[Either[Boolean, Boolean]]]
    val d: Arbitrary[(Either[Boolean, Boolean]) => Int] = implicitly[Arbitrary[(Either[Boolean, Boolean]) => Int]]
    val e: Eq[Tuple2K[Pair, Pair, Int]] = Eq[Tuple2K[Pair, Pair, Int]]
    val f: Eq[Int] = Eq[Int]

    checkAll(
      "Representable[Tuple2K[Pair, Pair, *]]",
      RepresentableTests[Tuple2K[Pair, Pair, *], Either[Boolean, Boolean]].representable[Int](
        a,
        b,
        c,
        d,
        e,
        f
      )
    )
    checkAll("Representable[Tuple2K[Pair, Pair, *]]",
             SerializableTests.serializable(Representable[Tuple2K[Pair, Pair, *]])
    )
  }

  {
    implicit val monoidK: MonoidK[ListWrapper] = ListWrapper.monoidK
    checkAll("Tuple2K[ListWrapper, ListWrapper, *]", MonoidKTests[Tuple2K[ListWrapper, ListWrapper, *]].monoidK[Int])
    checkAll("MonoidK[Tuple2K[ListWrapper, ListWrapper, *]]",
             SerializableTests.serializable(MonoidK[Tuple2K[ListWrapper, ListWrapper, *]])
    )
  }

  {
    implicit val semigroupK: SemigroupK[ListWrapper] = ListWrapper.semigroupK
    checkAll("Tuple2K[ListWrapper, ListWrapper, *]",
             SemigroupKTests[Tuple2K[ListWrapper, ListWrapper, *]].semigroupK[Int]
    )
    checkAll("SemigroupK[Tuple2K[ListWrapper, ListWrapper, *]]",
             SerializableTests.serializable(SemigroupK[Tuple2K[ListWrapper, ListWrapper, *]])
    )
  }

  {
    implicit val apply: Apply[ListWrapper] = ListWrapper.applyInstance
    implicit val iso: Isomorphisms[Tuple2K[ListWrapper, ListWrapper, *]] =
      Isomorphisms.invariant[Tuple2K[ListWrapper, ListWrapper, *]]
    checkAll("Tuple2K[ListWrapper, ListWrapper, *]",
             ApplyTests[Tuple2K[ListWrapper, ListWrapper, *]].apply[Int, Int, Int]
    )
    checkAll("Apply[Tuple2K[ListWrapper, ListWrapper, *]]",
             SerializableTests.serializable(Apply[Tuple2K[ListWrapper, ListWrapper, *]])
    )
  }

  {
    checkAll("Tuple2K[Option, Validated[Int, *], *]",
             CommutativeApplyTests[Tuple2K[Option, Validated[Int, *], *]].commutativeApply[Int, Int, Int]
    )
    checkAll("Apply[Tuple2K[Option, Validated[Int, *], *]]",
             SerializableTests.serializable(CommutativeApply[Tuple2K[Option, Validated[Int, *], *]])
    )
  }

  {
    checkAll("Tuple2K[Option, Validated[Int, *], *]",
             CommutativeApplicativeTests[Tuple2K[Option, Validated[Int, *], *]].commutativeApplicative[Int, Int, Int]
    )
    checkAll("Applicative[Tuple2K[Option, Validated[Int, *], *]]",
             SerializableTests.serializable(CommutativeApplicative[Tuple2K[Option, Validated[Int, *], *]])
    )
  }

  {
    implicit val functor: Functor[ListWrapper] = ListWrapper.functor
    checkAll("Tuple2K[ListWrapper, ListWrapper, *]",
             FunctorTests[Tuple2K[ListWrapper, ListWrapper, *]].functor[Int, Int, Int]
    )
    checkAll("Functor[Tuple2K[ListWrapper, ListWrapper, *]]",
             SerializableTests.serializable(Functor[Tuple2K[ListWrapper, ListWrapper, *]])
    )
  }

  {
    implicit val monad: Monad[ListWrapper] = ListWrapper.monad
    implicit val iso: Isomorphisms[Tuple2K[ListWrapper, ListWrapper, *]] =
      Isomorphisms.invariant[Tuple2K[ListWrapper, ListWrapper, *]]
    checkAll("Tuple2K[ListWrapper, ListWrapper, *]",
             MonadTests[Tuple2K[ListWrapper, ListWrapper, *]].monad[Int, Int, Int]
    )
    checkAll("Monad[Tuple2K[ListWrapper, ListWrapper, *]]",
             SerializableTests.serializable(Monad[Tuple2K[ListWrapper, ListWrapper, *]])
    )
  }

  {
    implicit val foldable: Foldable[ListWrapper] = ListWrapper.foldable
    checkAll("Tuple2K[ListWrapper, ListWrapper, *]",
             FoldableTests[Tuple2K[ListWrapper, ListWrapper, *]].foldable[Int, Int]
    )
    checkAll("Foldable[Tuple2K[ListWrapper, ListWrapper, *]]",
             SerializableTests.serializable(Foldable[Tuple2K[ListWrapper, ListWrapper, *]])
    )
  }

  {
    implicit val traverse: Traverse[ListWrapper] = ListWrapper.traverse
    checkAll("Tuple2K[ListWrapper, ListWrapper, *]",
             TraverseTests[Tuple2K[ListWrapper, ListWrapper, *]].traverse[Int, Int, Int, Int, Option, Option]
    )
    checkAll("Traverse[Tuple2K[ListWrapper, ListWrapper, *]]",
             SerializableTests.serializable(Traverse[Tuple2K[ListWrapper, ListWrapper, *]])
    )
  }

  {
    implicit val alternative: Alternative[ListWrapper] = ListWrapper.alternative
    implicit val iso: Isomorphisms[Tuple2K[ListWrapper, ListWrapper, *]] =
      Isomorphisms.invariant[Tuple2K[ListWrapper, ListWrapper, *]]
    checkAll("Tuple2K[ListWrapper, ListWrapper, *]",
             AlternativeTests[Tuple2K[ListWrapper, ListWrapper, *]].alternative[Int, Int, Int]
    )
    checkAll("Alternative[Tuple2K[ListWrapper, ListWrapper, *]]",
             SerializableTests.serializable(Alternative[Tuple2K[ListWrapper, ListWrapper, *]])
    )
  }

  {
    implicit val E: Eq[ListWrapper[Int]] = ListWrapper.eqv[Int]
    implicit val O: Order[ListWrapper[Int]] = ListWrapper.order[Int]
    implicit val P: PartialOrder[ListWrapper[Int]] = ListWrapper.partialOrder[Int]

    checkAll("Tuple2K[ListWrapper, ListWrapper, Int]", EqTests[Tuple2K[ListWrapper, ListWrapper, Int]].eqv)
    checkAll("Tuple2K[ListWrapper, ListWrapper, Int]", OrderTests[Tuple2K[ListWrapper, ListWrapper, Int]].order)
    checkAll("Tuple2K[ListWrapper, ListWrapper, Int]",
             PartialOrderTests[Tuple2K[ListWrapper, ListWrapper, Int]].partialOrder
    )
  }

  {
    checkAll("Tuple2K[Function0, Function0, *]",
             DistributiveTests[Tuple2K[Function0, Function0, *]].distributive[Int, Int, Int, Option, Function0]
    )
    checkAll("Distributive[Tuple2K[Function0, Function0, *]]",
             SerializableTests.serializable(Distributive[Tuple2K[Function0, Function0, *]])
    )
  }

  test("show") {
    forAll { (l1: Option[Int], l2: Option[Int]) =>
      val tuple = Tuple2K(l1, l2)

      assert(
        Show[Tuple2K[Option, Option, Int]].show(tuple) ===
          s"Tuple2K(${Show[Option[Int]].show(l1)}, ${Show[Option[Int]].show(l2)})"
      )
    }
  }

  test("double swap is identity") {
    forAll { (l1: Option[String], l2: List[String]) =>
      val tuple = Tuple2K(l1, l2)

      assert(tuple.swap.swap === tuple)
    }
  }

  test("firstK is consistent with first") {
    forAll { (tuple: Tuple2K[Option, Option, String]) =>
      assert(Tuple2K.firstK(tuple) === tuple.first)
    }
  }

  test("secondK is consistent with second") {
    forAll { (tuple: Tuple2K[Option, Option, String]) =>
      assert(Tuple2K.secondK(tuple) === tuple.second)
    }
  }

}
