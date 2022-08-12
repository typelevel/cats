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

import cats.{~>, Contravariant}
import cats.arrow.FunctionK
import cats.instances.all._
import cats.kernel.Eq
import cats.laws.discipline.{ContravariantTests, SerializableTests}
import cats.tests.CatsSuite
import org.scalacheck.Arbitrary
import cats.syntax.eq._
import org.scalacheck.Prop._

class ContravariantCoyonedaSuite extends CatsSuite {

  // If we can generate functions we can generate an interesting ContravariantCoyoneda.
  implicit def contravariantCoyonedaArbitrary[F[_], A, T](implicit
    F: Arbitrary[A => T]
  ): Arbitrary[ContravariantCoyoneda[* => T, A]] =
    Arbitrary(F.arbitrary.map(ContravariantCoyoneda.lift[* => T, A](_)))

  // We can't really test that functions are equal but we can try it with a bunch of test data.
  implicit def contravariantCoyonedaEq[A: Arbitrary, T](implicit eqft: Eq[T]): Eq[ContravariantCoyoneda[* => T, A]] =
    (cca, ccb) =>
      Arbitrary.arbitrary[List[A]].sample.get.forall { a =>
        eqft.eqv(cca.run.apply(a), ccb.run.apply(a))
      }

  // This instance cannot be summoned implicitly. This is not specific to contravariant coyoneda;
  // it doesn't work for Functor[Coyoneda[* => String, *]] either.
  implicit val contravariantContravariantCoyonedaToString: Contravariant[ContravariantCoyoneda[* => String, *]] =
    ContravariantCoyoneda.catsFreeContravariantFunctorForContravariantCoyoneda[* => String]

  checkAll("ContravariantCoyoneda[* => String, Int]",
           ContravariantTests[ContravariantCoyoneda[* => String, *]].contravariant[Int, Int, Int]
  )
  checkAll("Contravariant[ContravariantCoyoneda[Option, *]]",
           SerializableTests.serializable(Contravariant[ContravariantCoyoneda[Option, *]])
  )

  test("mapK and run is same as applying natural trans") {
    forAll { (b: Boolean) =>
      val nt = new ((* => String) ~> (* => Int)) {
        def apply[A](f: A => String): A => Int = s => f(s).length
      }
      val o = (b: Boolean) => b.toString
      val c = ContravariantCoyoneda.lift[* => String, Boolean](o)
      c.mapK[* => Int](nt).run.apply(b) === nt(o).apply(b)
    }
  }

  test("contramap order") {
    ContravariantCoyoneda
      .lift[* => Int, String](_.count(_ == 'x'))
      .contramap((s: String) => s + "x")
      .contramap((s: String) => s * 3)
      .run
      .apply("foo") === 3
  }

  test("stack-safe contramapmap") {
    def loop(n: Int, acc: ContravariantCoyoneda[* => Int, Int]): ContravariantCoyoneda[* => Int, Int] =
      if (n <= 0) acc
      else loop(n - 1, acc.contramap((_: Int) + 1))
    loop(20000, ContravariantCoyoneda.lift[* => Int, Int](a => a)).run.apply(10)
  }

  test("run, foldMap consistent") {
    forAll {
      (
        c: ContravariantCoyoneda[* => Int, String],
        f: Byte => String,
        g: Float => Byte,
        s: Float
      ) =>
        val cʹ = c.contramap(f).contramap(g) // just to ensure there's some structure
        val h = cʹ.foldMap[* => Int](FunctionK.id[* => Int])
        cʹ.run.apply(s) === h(s)
    }
  }

}
