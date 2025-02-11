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

package cats.free

import cats.Functor
import cats.instances.all.*
import cats.kernel.Eq
import cats.laws.discipline.{FunctorTests, SerializableTests}
import cats.tests.CatsSuite
import org.scalacheck.Arbitrary
import cats.syntax.eq.*
import org.scalacheck.Prop.*

class YonedaSuite extends CatsSuite {
  implicit def yonedaArbitrary[F[_]: Functor, A](implicit F: Arbitrary[F[A]]): Arbitrary[Yoneda[F, A]] =
    Arbitrary(F.arbitrary.map(Yoneda(_)))

  implicit def yonedaEq[F[_]: Functor, A](implicit FA: Eq[F[A]]): Eq[Yoneda[F, A]] =
    Eq.by(_.run)

  checkAll("Yoneda[Option, *]", FunctorTests[Yoneda[Option, *]].functor[Int, Int, Int])
  checkAll("Functor[Yoneda[Option, *]]", SerializableTests.serializable(Functor[Yoneda[Option, *]]))

  property("toCoyoneda and then toYoneda is identity") {
    forAll { (y: Yoneda[Option, Int]) =>
      assert(y.toCoyoneda.toYoneda === y)
    }
  }
}
