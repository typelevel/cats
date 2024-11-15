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

import cats.{Bifoldable, Bifunctor, Bitraverse, Foldable, Functor, Traverse}
import cats.arrow.Profunctor
import cats.data.Binested
import cats.kernel.Eq
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import cats.syntax.bifunctor.*
import cats.syntax.binested.*
import cats.syntax.eq.*
import org.scalacheck.Prop.*
import org.scalacheck.Test.Parameters

class BinestedSuite extends CatsSuite {
  // we have a lot of generated lists of lists in these tests. We have to tell
  // ScalaCheck to calm down a bit so we don't hit memory and test duration
  // issues.
  implicit override val scalaCheckTestParameters: Parameters =
    Parameters.default.withMinSuccessfulTests(20).withMaxSize(Parameters.default.minSize + 5)

  {
    // Bifunctor + Functor + Functor = Bifunctor
    implicit val instance: Functor[ListWrapper] = ListWrapper.functor
    checkAll(
      "Binested[Either, ListWrapper, Option, *, *]",
      BifunctorTests[Binested[Either, ListWrapper, Option, *, *]].bifunctor[Int, Int, Int, String, String, String]
    )
    checkAll("Bifunctor[Binested[Either, ListWrapper, Option, *, *]]",
             SerializableTests.serializable(Bifunctor[Binested[Either, ListWrapper, Option, *, *]])
    )
  }

  {
    // Profunctor + Functor + Functor = Profunctor
    implicit val instance: Functor[OptionWrapper] = OptionWrapper.functor
    Eq[OptionWrapper[MiniInt] => Option[Int]]
    checkAll(
      "Binested[Function1, OptionWrapper, Option, *, *]",
      ProfunctorTests[Binested[Function1, OptionWrapper, Option, *, *]]
        .profunctor[MiniInt, Int, Int, String, String, String]
    )
    checkAll(
      "Profunctor[Binested[Function1, OptionWrapper, Option, *, *]]",
      SerializableTests.serializable(Profunctor[Binested[Function1, OptionWrapper, Option, *, *]])
    )
  }

  {
    // Bifoldable + foldable + foldable = Bifoldable
    implicit val instance: Foldable[ListWrapper] = ListWrapper.foldable
    checkAll("Binested[Either, ListWrapper, ListWrapper, *, *]",
             BifoldableTests[Binested[Either, ListWrapper, ListWrapper, *, *]].bifoldable[Int, Int, Int]
    )
    checkAll(
      "Bifoldable[Binested[Either, ListWrapper, ListWrapper, *, *]]",
      SerializableTests.serializable(Bifoldable[Binested[Either, ListWrapper, ListWrapper, *, *]])
    )
  }

  {
    // Bitraverse + traverse + traverse = Bitraverse
    implicit val instance: Traverse[ListWrapper] = ListWrapper.traverse
    checkAll(
      "Binested[Either, ListWrapper, ListWrapper, *, *]",
      BitraverseTests[Binested[Either, ListWrapper, ListWrapper, *, *]]
        .bitraverse[Option, Int, Int, Int, String, String, String]
    )
    checkAll(
      "Bitraverse[Binested[Either, ListWrapper, ListWrapper, *, *]]",
      SerializableTests.serializable(Bitraverse[Binested[Either, ListWrapper, ListWrapper, *, *]])
    )
  }

  test("simple syntax-based usage") {
    forAll { (value: (Option[Int], List[Int])) =>
      // TODO something is wrong with inference in Dotty here, bug?
      val binested = value.binested
      assert(binested.bimap(_.toString, _.toString).value === (value.bimap(_.map(_.toString), _.map(_.toString))))
    }
  }
}
