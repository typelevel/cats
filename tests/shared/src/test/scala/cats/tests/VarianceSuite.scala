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

import cats.arrow.Profunctor
import cats.conversions.all._
import cats.syntax.all._
import cats.{Bifunctor, Contravariant, Functor}

class VarianceSuite extends CatsSuite {

  sealed trait FooBar
  sealed trait Foo extends FooBar
  case class Bar(x: Int) extends Foo
  case object Baz extends Foo

  test("Auto-variance should infer subtypes correctly") {
    def shouldInfer[F[_]: Functor](fi: F[Int]) =
      fi.map(i => if (true) Left(Bar(i)) else Right(Baz))

    def inferred[F[_]: Functor](fi: F[Int]): F[Either[Foo, Foo]] = shouldInfer[F](fi)
  }

  test("Auto-variance should infer supertypes correctly") {
    def shouldCompile[F[_]: Contravariant](fi: F[Foo])(f: F[Bar] => Int) =
      f(fi)
  }

  test("Auto-variance should widen a bifunctor automatically") {
    def shouldInfer[F[_, _]: Bifunctor](fi: F[Int, Int]) =
      fi.bimap(i => if (true) Left(Bar(i)) else Right(Baz), i => if (true) Left(Bar(i)) else Right(Baz))

    def inferred[F[_, _]: Bifunctor](fi: F[Int, Int]): F[Either[Foo, Foo], Either[Foo, Foo]] = shouldInfer[F](fi)
  }

  test("Auto-variance should left widen a bifunctor automatically") {
    def shouldInfer[F[_, _]: Bifunctor](fi: F[Int, Int]) =
      fi.bimap(identity, i => if (true) Left(Bar(i)) else Right(Baz))

    def inferred[F[_, _]: Bifunctor](fi: F[Int, Int]): F[Int, Either[Foo, Foo]] = shouldInfer[F](fi)
  }

  test("Auto-variance should right widen a bifunctor automatically") {
    def shouldInfer[F[_, _]: Bifunctor](fi: F[Int, Int]) =
      fi.bimap(i => if (true) Left(Bar(i)) else Right(Baz), identity)

    def inferred[F[_, _]: Bifunctor](fi: F[Int, Int]): F[Either[Foo, Foo], Int] = shouldInfer[F](fi)
  }

  test("Auto-variance auto adjust a profunctor's variance") {
    def shouldInfer[F[_, _]: Profunctor](fi: F[Int, Int]) =
      fi.dimap((_: Either[FooBar, FooBar]) => 1)(i => if (true) Left(Bar(i)) else Right(Baz))

    def inferred[F[_, _]: Profunctor](fi: F[Int, Int]): F[Either[Foo, Foo], Either[Foo, Foo]] = shouldInfer[F](fi)
  }

  test("Auto-variance should widen the second type parameter of a profunctor automatically") {
    def shouldInfer[F[_, _]: Profunctor](fi: F[Int, Int]) =
      fi.dimap(identity[Int])(i => if (true) Left(Bar(i)) else Right(Baz))

    def inferred[F[_, _]: Profunctor](fi: F[Int, Int]): F[Int, Either[Foo, Foo]] = shouldInfer[F](fi)
  }

  test("Auto-variance should narrow the first type parameter of a profunctor automatically") {
    def shouldInfer[F[_, _]: Profunctor](fi: F[Int, Int]) =
      fi.dimap((_: Either[FooBar, FooBar]) => 1)(identity[Int])

    def inferred[F[_, _]: Profunctor](fi: F[Int, Int]): F[Either[Foo, Foo], Int] = shouldInfer[F](fi)
  }

}
