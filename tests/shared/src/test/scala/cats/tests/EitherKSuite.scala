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
import cats.data.EitherK
import cats.kernel.laws.discipline.EqTests
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import cats.syntax.eq.*
import org.scalacheck.Prop.*

class EitherKSuite extends CatsSuite {

  checkAll("EitherK[Option, Option, *]",
           TraverseTests[EitherK[Option, Option, *]].traverse[Int, Int, Int, Int, Option, Option]
  )
  checkAll("Traverse[EitherK[Option, Option, *]]", SerializableTests.serializable(Traverse[EitherK[Option, Option, *]]))

  {
    implicit val foldable: Foldable[EitherK[Option, Option, *]] = EitherK.catsDataFoldableForEitherK[Option, Option]
    checkAll("EitherK[Option, Option, *]", FoldableTests[EitherK[Option, Option, *]].foldable[Int, Int])
    checkAll("Foldable[EitherK[Option, Option, *]]",
             SerializableTests.serializable(Foldable[EitherK[Option, Option, *]])
    )
  }

  checkAll("EitherK[Eval, Eval, *]", ComonadTests[EitherK[Eval, Eval, *]].comonad[Int, Int, Int])
  checkAll("Comonad[EitherK[Eval, Eval, *]]", SerializableTests.serializable(Comonad[EitherK[Eval, Eval, *]]))

  {
    implicit val coflatMap: CoflatMap[EitherK[Eval, Eval, *]] = EitherK.catsDataCoflatMapForEitherK[Eval, Eval]
    checkAll("EitherK[Eval, Eval, *]", CoflatMapTests[EitherK[Eval, Eval, *]].coflatMap[Int, Int, Int])
    checkAll("CoflatMap[EitherK[Eval, Eval, *]]", SerializableTests.serializable(CoflatMap[EitherK[Eval, Eval, *]]))
  }

  checkAll("EitherK[Option, Option, Int]", EqTests[EitherK[Option, Option, Int]].eqv)
  checkAll("Eq[EitherK[Option, Option, Int]]", SerializableTests.serializable(Eq[EitherK[Option, Option, Int]]))

  checkAll("EitherK[Show, Show, *]", ContravariantTests[EitherK[Show, Show, *]].contravariant[MiniInt, Int, Boolean])
  checkAll("Contravariant[EitherK[Show, Show, *]]",
           SerializableTests.serializable(Contravariant[EitherK[Show, Show, *]])
  )

  test("double swap is identity") {
    forAll { (x: EitherK[Option, Option, Int]) =>
      assert(x.swap.swap === x)
    }
  }

  test("swap negates isLeft/isRight") {
    forAll { (x: EitherK[Option, Option, Int]) =>
      assert(x.isLeft =!= (x.swap.isLeft))
      assert(x.isRight =!= (x.swap.isRight))
    }
  }

  test("isLeft consistent with isRight") {
    forAll { (x: EitherK[Option, Option, Int]) =>
      assert(x.isLeft =!= (x.isRight))
    }
  }

  test("toValidated + toEither is identity") {
    forAll { (x: EitherK[Option, List, Int]) =>
      assert(x.toValidated.toEither === (x.run))
    }
  }
}
