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
import cats.arrow.FunctionK
import cats.instances.all.*
import cats.kernel.Eq
import cats.laws.discipline.{FunctorTests, SerializableTests}
import cats.tests.CatsSuite
import org.scalacheck.Arbitrary
import cats.syntax.eq.*
import org.scalacheck.Prop.*

class CoyonedaSuite extends CatsSuite {
  implicit def coyonedaArbitrary[F[_]: Functor, A: Arbitrary](implicit F: Arbitrary[F[A]]): Arbitrary[Coyoneda[F, A]] =
    Arbitrary(F.arbitrary.map(Coyoneda.lift))

  implicit def coyonedaEq[F[_]: Functor, A](implicit FA: Eq[F[A]]): Eq[Coyoneda[F, A]] =
    Eq.by(_.run)

  checkAll("Coyoneda[Option, *]", FunctorTests[Coyoneda[Option, *]].functor[Int, Int, Int])
  checkAll("Functor[Coyoneda[Option, *]]", SerializableTests.serializable(Functor[Coyoneda[Option, *]]))

  test("toYoneda and then toCoyoneda is identity") {
    forAll { (y: Coyoneda[Option, Int]) =>
      assert(y === y.toYoneda.toCoyoneda)
    }
  }

  test("mapK and run is same as applying natural trans") {
    val nt = new FunctionK[Option, List] { def apply[A](a: Option[A]): List[A] = a.toList }
    val o = Option("hello")
    val c = Coyoneda.lift(o)
    assert(c.mapK(nt).run === nt(o))
  }

  test("map order") {
    Coyoneda
      .lift[Option, Int](Some(0))
      .map(_ + 1)
      .map(_ * 3)
      .run === Some(3)
  }

  test("stack-safe map") {
    def loop(n: Int, acc: Coyoneda[Option, Int]): Coyoneda[Option, Int] =
      if (n <= 0) acc
      else loop(n - 1, acc.map(_ + 1))

    loop(20000, Coyoneda.lift[Option, Int](Some(1))).run
  }
}
