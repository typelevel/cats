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

import cats.Alternative
import cats.Applicative
import cats.Foldable
import cats.Functor
import cats.Monad
import cats.SemigroupK
import cats.Traverse
import cats.data.OneAnd
import cats.kernel.Eq
import cats.kernel.Order
import cats.kernel.PartialOrder
import cats.kernel.laws.discipline.EqTests
import cats.kernel.laws.discipline.OrderTests
import cats.kernel.laws.discipline.PartialOrderTests
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.syntax.foldable._
import cats.syntax.order._
import org.scalacheck.Prop._
import org.scalacheck.Test.Parameters

class OneAndSuite extends CatsSuite {
  // Lots of collections here.. telling ScalaCheck to calm down a bit
  implicit override val scalaCheckTestParameters: Parameters =
    Parameters.default.withMinSuccessfulTests(20).withMaxSize(Parameters.default.minSize + 5)

  // Test kernel instances
  {
    import Helpers.Eqed

    checkAll("OneAnd[F, A]", EqTests[OneAnd[ListWrapper, Eqed]].eqv)
    checkAll("Eq[OneAnd[F, A]]", SerializableTests.serializable(Eq[OneAnd[ListWrapper, Eqed]]))

    property("Eq[OneAnd[F, A]]: must be consistent with Eq[Tuple2[A, F[A]]]") {
      forAll { (x: OneAnd[ListWrapper, Eqed], y: OneAnd[ListWrapper, Eqed]) =>
        assertEquals(x.eqv(y), (x.head, x.tail).eqv((y.head, y.tail)))
      }
    }
  }
  {
    import Helpers.POrd

    implicit val partialOrder: PartialOrder[ListWrapper[POrd]] = ListWrapper.partialOrder
    checkAll("OneAnd[F, A]", PartialOrderTests[OneAnd[ListWrapper, POrd]].partialOrder)
    checkAll("PartialOrder[OneAnd[F, A]]", SerializableTests.serializable(PartialOrder[OneAnd[ListWrapper, POrd]]))

    property("PartialOrder[OneAnd[F, A]]: must be consistent with PartialOrder[Tuple2[A, F[A]]]") {
      forAll { (x: OneAnd[ListWrapper, POrd], y: OneAnd[ListWrapper, POrd]) =>
        // `NaN` cannot be compared directly; hence using `partialComparison` instead of `partialCompare`.
        assertEquals(x.partialComparison(y), (x.head, x.tail).partialComparison((y.head, y.tail)))
      }
    }
  }
  {
    import Helpers.Ord

    implicit val order: Order[ListWrapper[Ord]] = ListWrapper.order
    checkAll("OneAnd[F, A]", OrderTests[OneAnd[ListWrapper, Ord]].order)
    checkAll("Order[OneAnd[F, A]]", SerializableTests.serializable(Order[OneAnd[ListWrapper, Ord]]))

    property("Order[OneAnd[F, A]]: must be consistent with Order[Tuple2[A, F[A]]]") {
      forAll { (x: OneAnd[ListWrapper, Ord], y: OneAnd[ListWrapper, Ord]) =>
        assertEquals(x.compare(y), (x.head, x.tail).compare((y.head, y.tail)))
      }
    }
  }

  {
    implicit val traverse: Traverse[OneAnd[ListWrapper, *]] = OneAnd.catsDataTraverseForOneAnd(ListWrapper.traverse)
    checkAll("OneAnd[ListWrapper, Int] with Option",
             TraverseTests[OneAnd[ListWrapper, *]].traverse[Int, Int, Int, Int, Option, Option]
    )
    checkAll("Traverse[OneAnd[ListWrapper, A]]", SerializableTests.serializable(Traverse[OneAnd[ListWrapper, *]]))
  }

  implicit val iso: Isomorphisms[OneAnd[ListWrapper, *]] =
    Isomorphisms.invariant[OneAnd[ListWrapper, *]](OneAnd.catsDataFunctorForOneAnd(ListWrapper.functor))

  // Test instances that have more general constraints
  {
    implicit val monad: Monad[ListWrapper] = ListWrapper.monad
    implicit val alt: Alternative[ListWrapper] = ListWrapper.alternative
    checkAll("OneAnd[ListWrapper, Int]", MonadTests[OneAnd[ListWrapper, *]].monad[Int, Int, Int])
    checkAll("MonadTests[OneAnd[ListWrapper, A]]", SerializableTests.serializable(Monad[OneAnd[ListWrapper, *]]))
  }

  {
    implicit val alternative: Alternative[ListWrapper] = ListWrapper.alternative
    checkAll("OneAnd[ListWrapper, Int]", ApplicativeTests[OneAnd[ListWrapper, *]].applicative[Int, Int, Int])
    checkAll("Applicative[OneAnd[ListWrapper, A]]", SerializableTests.serializable(Applicative[OneAnd[ListWrapper, *]]))
  }

  {
    implicit val functor: Functor[ListWrapper] = ListWrapper.functor
    checkAll("OneAnd[ListWrapper, Int]", FunctorTests[OneAnd[ListWrapper, *]].functor[Int, Int, Int])
    checkAll("Functor[OneAnd[ListWrapper, A]]", SerializableTests.serializable(Functor[OneAnd[ListWrapper, *]]))
  }

  {
    implicit val alternative: Alternative[ListWrapper] = ListWrapper.alternative
    checkAll("OneAnd[ListWrapper, Int]", SemigroupKTests[OneAnd[ListWrapper, *]].semigroupK[Int])
    checkAll("SemigroupK[OneAnd[ListWrapper, A]]", SerializableTests.serializable(SemigroupK[OneAnd[ListWrapper, *]]))
  }

  {
    implicit val foldable: Foldable[ListWrapper] = ListWrapper.foldable
    checkAll("OneAnd[ListWrapper, Int]", FoldableTests[OneAnd[ListWrapper, *]].foldable[Int, Int])
    checkAll("Foldable[OneAnd[ListWrapper, A]]", SerializableTests.serializable(Foldable[OneAnd[ListWrapper, *]]))
  }

  test("size is consistent with toList.size") {
    forAll { (oa: OneAnd[Vector, Int]) =>
      assert(oa.size === (oa.toList.size.toLong))
    }
  }

}
