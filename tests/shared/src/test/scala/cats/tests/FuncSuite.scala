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

import cats.{Applicative, Apply, Contravariant, Functor, Semigroupal, Show}
import cats.data.{AppFunc, Func}
import cats.data.Func.appFunc
import cats.kernel.Eq
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.syntax.eq.*

class FuncSuite extends CatsSuite {
  import cats.laws.discipline.eq.*
  implicit def funcEq[F[_], A, B](implicit ev: Eq[A => F[B]]): Eq[Func[F, A, B]] =
    Eq.by[Func[F, A, B], A => F[B]](_.run)
  implicit def appFuncEq[F[_], A, B](implicit ev: Eq[A => F[B]]): Eq[AppFunc[F, A, B]] =
    Eq.by[AppFunc[F, A, B], A => F[B]](_.run)

  implicit val iso: Isomorphisms[Func[Option, Int, *]] = Isomorphisms.invariant[Func[Option, Int, *]]

  checkAll("Func[Option, MiniInt, Int]", SemigroupalTests[Func[Option, MiniInt, *]].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Func[Option, Int, *]]", SerializableTests.serializable(Semigroupal[Func[Option, Int, *]]))

  {
    implicit val catsDataApplicativeForFunc: Applicative[Func[Option, Int, *]] =
      Func.catsDataApplicativeForFunc[Option, Int]
    checkAll("Func[Option, MiniInt, Int]", ApplicativeTests[Func[Option, MiniInt, *]].applicative[Int, Int, Int])
    checkAll("Applicative[Func[Option, Int, *]]", SerializableTests.serializable(Applicative[Func[Option, Int, *]]))
  }

  {
    implicit val catsDataApplyForFunc: Apply[Func[Option, MiniInt, *]] = Func.catsDataApplyForFunc[Option, MiniInt]
    checkAll("Func[Option, MiniInt, Int]", ApplyTests[Func[Option, MiniInt, *]].apply[Int, Int, Int])
    checkAll("Apply[Func[Option, Int, *]]", SerializableTests.serializable(Apply[Func[Option, Int, *]]))
  }

  {
    implicit val catsDataFunctorForFunc: Functor[Func[Option, Int, *]] = Func.catsDataFunctorForFunc[Option, Int]
    checkAll("Func[Option, MiniInt, Int]", FunctorTests[Func[Option, MiniInt, *]].functor[Int, Int, Int])
    checkAll("Functor[Func[Option, Int, *]]", SerializableTests.serializable(Functor[Func[Option, Int, *]]))
  }

  {
    implicit val funcContravariant: Contravariant[Func[Show, *, MiniInt]] =
      Func.catsDataContravariantForFunc[Show, MiniInt]
    checkAll("Func[Show, MiniInt, MiniInt]",
             ContravariantTests[Func[Show, *, MiniInt]].contravariant[MiniInt, MiniInt, MiniInt]
    )
    checkAll("Contravariant[Func[Show, *, Int]]", SerializableTests.serializable(Contravariant[Func[Show, *, Int]]))
  }

  {
    implicit val appFuncApp: Applicative[AppFunc[Option, Int, *]] = AppFunc.appFuncApplicative[Option, Int]
    implicit val iso: Isomorphisms[AppFunc[Option, Int, *]] = Isomorphisms.invariant[AppFunc[Option, Int, *]]
    checkAll("AppFunc[Option, MiniInt, MiniInt]",
             ApplicativeTests[AppFunc[Option, MiniInt, *]].applicative[MiniInt, MiniInt, MiniInt]
    )
    checkAll("Applicative[AppFunc[Option, Int, *]]",
             SerializableTests.serializable(Applicative[AppFunc[Option, Int, *]])
    )
  }

  test("product") {
    val f = appFunc { (x: Int) =>
      Some(x + 10): Option[Int]
    }
    val g = appFunc { (x: Int) =>
      List(x * 2)
    }
    val h = f.product(g)
    val x = h.run(1)
    assert((x.first, x.second) === ((Some(11), List(2))))
  }

  test("traverse") {
    val f = Func.appFunc { (x: Int) =>
      Some(x + 10): Option[Int]
    }
    val xs = f.traverse(List(1, 2, 3))
    assert(xs === (Some(List(11, 12, 13))))
  }
}
