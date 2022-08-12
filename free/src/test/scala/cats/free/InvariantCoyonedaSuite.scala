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

import cats.~>
import cats.arrow.FunctionK
import cats.Invariant
import cats.Semigroup
import cats.instances.all._
import cats.kernel.Eq
import cats.laws.discipline.{InvariantTests, SerializableTests}
import cats.tests.CatsSuite
import org.scalacheck.Arbitrary
import cats.syntax.eq._
import org.scalacheck.Prop._

class InvariantCoyonedaSuite extends CatsSuite {

  type Magma[A] = (A, A) => A
  implicit def semigroupIsMagma[A: Semigroup]: Magma[A] = Semigroup[A].combine
  implicit object invariantForMagma extends Invariant[Magma] {
    override def imap[A, B](fa: Magma[A])(f: A => B)(g: B => A): Magma[B] =
      (x, y) => f(fa(g(x), g(y)))
  }

  // If we can generate functions we can generate an interesting InvariantCoyoneda.
  implicit def invariantCoyonedaArbitrary[F[_], A: Magma, T](implicit
    F: Arbitrary[(A, A) => A]
  ): Arbitrary[InvariantCoyoneda[Magma, A]] =
    Arbitrary(F.arbitrary.map(InvariantCoyoneda.lift[Magma, A]))

  // We can't really test that magmas are equal but we can try it with a bunch of test data.
  implicit def invariantCoyonedaEq[A: Arbitrary: Eq]: Eq[InvariantCoyoneda[Magma, A]] =
    (cca, ccb) =>
      Arbitrary.arbitrary[List[(A, A)]].sample.get.forall { case (x, y) =>
        cca.run.apply(x, y) == ccb.run.apply(x, y)
      }

  // Needed to help implicit resolution?
  implicit val invariantCoyonedaMagma: Invariant[InvariantCoyoneda[Magma, *]] =
    InvariantCoyoneda.catsFreeInvariantFunctorForInvariantCoyoneda[Magma]

  checkAll("InvariantCoyoneda[Magma, Int]",
           InvariantTests[InvariantCoyoneda[Magma, *]].invariant[List[Int], List[Int], List[Int]]
  )
  checkAll("Invariant[InvariantCoyoneda[Magma, *]]",
           SerializableTests.serializable(Invariant[InvariantCoyoneda[Magma, *]])
  )

  test("mapK and run is same as applying natural trans") {
    forAll { (x: Option[Int], y: Option[Int]) =>
      val nt = new (Magma ~> Magma) {
        def apply[A](m: Magma[A]): Magma[A] = (x, y) => m(y, x)
      }
      val m: Magma[Option[Int]] = _.orElse(_)
      val c = InvariantCoyoneda.lift[Magma, Option[Int]](m)
      c.mapK[Magma](nt).run.apply(x, y) === nt(m).apply(x, y)
    }
  }

  test("imap order") {
    forAll { (x: String, y: String) =>
      InvariantCoyoneda
        .lift[Magma, String](_ + _)
        .imap(Some(_))(_.value)
        .imap(Right(_))(_.value)
        .run
        .apply(Right(Some(x)), Right(Some(y))) == Right(Some(x + y))
    }
  }

  test("stack-safe imapmap") {
    def loop(n: Int, acc: InvariantCoyoneda[Magma, Int]): InvariantCoyoneda[Magma, Int] =
      if (n <= 0) acc
      else loop(n - 1, acc.imap((_: Int) + 1)((_: Int) - 1))
    loop(20000, InvariantCoyoneda.lift[Magma, Int](_ + _)).run.apply(10, 11)
  }

  test("run, foldMap consistent") {
    forAll {
      (
        c: InvariantCoyoneda[Magma, String],
        f1: String => Byte,
        g1: Byte => String,
        f2: Byte => Float,
        g2: Float => Byte,
        s1: Float,
        s2: Float
      ) =>
        val cʹ = c.imap(f1)(g1).imap(f2)(g2) // just to ensure there's some structure
        val h = cʹ.foldMap[Magma](FunctionK.id[Magma])
        cʹ.run.apply(s1, s2) === h.apply(s1, s2)
    }
  }

}
