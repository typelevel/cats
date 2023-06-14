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

import cats._
import cats.data.{Const, NonEmptyList}
import cats.kernel.Semigroup
import cats.kernel.laws.discipline.{
  EqTests,
  HashTests,
  LowerBoundedTests,
  MonoidTests,
  OrderTests,
  PartialOrderTests,
  SemigroupTests,
  UpperBoundedTests
}
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.syntax.eq._
import cats.syntax.show._
import cats.tests.Helpers.{CMono, CSemi, Semi}
import org.scalacheck.Prop._

class ConstSuite extends CatsSuite {

  implicit val iso: Isomorphisms[Const[String, *]] =
    Isomorphisms.invariant[Const[String, *]](Const.catsDataTraverseForConst)

  checkAll("Const[String, Int]", SemigroupalTests[Const[String, *]].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Const[String, *]]", SerializableTests.serializable(Semigroupal[Const[String, *]]))

  checkAll("Const[String, Int]", ApplicativeTests[Const[String, *]].applicative[Int, Int, Int])
  checkAll("Applicative[Const[String, *]]", SerializableTests.serializable(Applicative[Const[String, *]]))

  checkAll("Const[String, Int] with Option",
           TraverseTests[Const[String, *]].traverse[Int, Int, Int, Int, Option, Option]
  )
  checkAll("Traverse[Const[String, *]]", SerializableTests.serializable(Traverse[Const[String, *]]))

  checkAll("Const[String, Int]", TraverseFilterTests[Const[String, *]].traverseFilter[Int, Int, Int])
  checkAll("TraverseFilter[Const[String, *]]", SerializableTests.serializable(TraverseFilter[Const[String, *]]))

  checkAll("Const[String, Int]", AlignTests[Const[String, *]].align[Int, Int, Int, Int])
  checkAll("Align[Const[String, *]]", SerializableTests.serializable(Align[Const[String, *]]))

  // Get Apply[Const[C : Semigroup, *]], not Applicative[Const[C : Monoid, *]]
  {
    implicit def nonEmptyListSemigroup[A]: Semigroup[NonEmptyList[A]] = SemigroupK[NonEmptyList].algebra
    implicit val iso: Isomorphisms[Const[NonEmptyList[String], *]] =
      Isomorphisms.invariant[Const[NonEmptyList[String], *]](Const.catsDataContravariantForConst)
    checkAll("Apply[Const[NonEmptyList[String], Int]]", ApplyTests[Const[NonEmptyList[String], *]].apply[Int, Int, Int])
    checkAll("Apply[Const[NonEmptyList[String], *]]",
             SerializableTests.serializable(Apply[Const[NonEmptyList[String], *]])
    )
  }

  // Algebra checks for Serializability of instances as part of the laws
  checkAll("Monoid[Const[Int, String]]", MonoidTests[Const[Int, String]].monoid)

  checkAll("Const[NonEmptyList[Int], String]", SemigroupTests[Const[NonEmptyList[Int], String]].semigroup)

  // Note while Eq is a superclass of PartialOrder and PartialOrder a superclass
  // of Order, you can get different instances with different (more general) constraints.
  // For instance, you can get an Order for Const if the first type parameter has an Order,
  // but you can also get just an Eq for Const if the first type parameter has just an Eq
  checkAll("Const[Map[Int, Int], String]", EqTests[Const[Map[Int, Int], String]].eqv)
  checkAll("PartialOrder[Const[Set[Int], String]]", PartialOrderTests[Const[Set[Int], String]].partialOrder)
  checkAll("Order[Const[Int, String]]", OrderTests[Const[Int, String]].order)
  checkAll("LowerBounded[Const[Int, String]]", LowerBoundedTests[Const[Int, String]].lowerBounded)
  checkAll("UpperBounded[Const[Int, String]]", UpperBoundedTests[Const[Int, String]].upperBounded)

  {
    implicitly[Invariant[Const[String, *]]]
    Invariant[Const[String, *]]

    checkAll("Const[String, Int]", InvariantTests[Const[String, *]].invariant[Int, Int, Int])
    checkAll("Invariant[Const[String, *]]", SerializableTests.serializable(Invariant[Const[String, *]]))
  }

  checkAll("Const[String, Int]", ContravariantTests[Const[String, *]].contravariant[Int, Int, Int])
  checkAll("Contravariant[Const[String, *]]", SerializableTests.serializable(Contravariant[Const[String, *]]))

  checkAll("ContravariantMonoidal[Const[Int, *]]",
           ContravariantMonoidalTests[Const[Int, *]].contravariantMonoidal[Int, Int, Int]
  )
  checkAll("ContravariantMonoidal[Const[Int, *]]", SerializableTests.serializable(ContravariantMonoidal[Const[Int, *]]))

  checkAll("ContravariantSemigroupal[Const[Semi, *]]",
           ContravariantSemigroupalTests[Const[Semi, *]].contravariantSemigroupal[Int, Int, Int]
  )
  checkAll("ContravariantSemigroupal[Const[Semi, *]]",
           SerializableTests.serializable(ContravariantSemigroupal[Const[Semi, *]])
  )

  checkAll("Const[*, *]", BifoldableTests[Const].bifoldable[Int, Int, Int])
  checkAll("Bifoldable[Const]", SerializableTests.serializable(Bifoldable[Const]))

  checkAll("InvariantMonoidal[Const[String, *]]",
           InvariantMonoidalTests[Const[String, *]].invariantMonoidal[Int, Int, Int]
  )
  checkAll("InvariantMonoidal[Const[String, *]]", SerializableTests.serializable(InvariantMonoidal[Const[String, *]]))

  test("show") {

    assert(Const(1).show === "Const(1)")

    forAll { (const: Const[Int, String]) =>
      assert(const.show.startsWith("Const(") === true)
      const.show.contains(const.getConst.show)
      assert(const.show === Show[Const[Int, String]].show(const))
      assert(const.show === const.retag[Boolean].show)
    }
  }

  checkAll("Const[String, Int]", FunctorTests[Const[String, *]].functor[Int, Int, Int])
  checkAll("Functor[Const[String, *]]", SerializableTests.serializable(Functor[Const[String, *]]))

  {
    implicit val iso: Isomorphisms[Const[CMono, *]] =
      Isomorphisms.invariant[Const[CMono, *]](Const.catsDataTraverseForConst)
    checkAll("Const[CMono, Int]", CommutativeApplicativeTests[Const[CMono, *]].commutativeApplicative[Int, Int, Int])
    checkAll("CommutativeApplicative[Const[CMono, *]]",
             SerializableTests.serializable(CommutativeApplicative[Const[CMono, *]])
    )
  }

  checkAll("Const[CSemi, Int]", CommutativeApplyTests[Const[CSemi, *]].commutativeApply[Int, Int, Int])
  checkAll("CommutativeApply[Const[CSemi, *]]", SerializableTests.serializable(CommutativeApply[Const[CSemi, *]]))

  checkAll("Hash[Const[Int, String]]", HashTests[Const[Int, String]].hash)
  checkAll("Hash[Const[Int, String]]", SerializableTests.serializable(Hash[Const[Int, String]]))

  checkAll("MonoidK[Const[Int, *]]", MonoidKTests[Const[Int, *]].monoidK[Int])
  checkAll("MonoidK[Const[Int, *]]", SerializableTests.serializable(MonoidK[Const[Int, *]]))

  checkAll("SemigroupK[Const[Int, *]]", SemigroupKTests[Const[Semi, *]].semigroupK[Int])
  checkAll("SemigroupK[Const[Int, *]]", SerializableTests.serializable(SemigroupK[Const[Semi, *]]))
}

object ConstSuite {
  def summonInstances[A, B: Hash](): Unit = {
    InvariantMonoidal[Const[Int, *]]
    Invariant[Const[A, *]]
    Functor[Const[A, *]]
    Eq[Const[B, Int]]
    ()
  }
}
