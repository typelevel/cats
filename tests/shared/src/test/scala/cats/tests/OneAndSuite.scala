/*
 * Copyright (c) 2022 Typelevel
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

import cats.{Alternative, Applicative, Foldable, Functor, Monad, SemigroupK, Traverse}
import cats.data.OneAnd
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.syntax.foldable._
import cats.syntax.eq._
import org.scalacheck.Prop._
import org.scalacheck.Test.Parameters

class OneAndSuite extends CatsSuite {
  // Lots of collections here.. telling ScalaCheck to calm down a bit
  implicit override val scalaCheckTestParameters: Parameters =
    Parameters.default.withMinSuccessfulTests(20).withMaxSize(Parameters.default.minSize + 5)

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
