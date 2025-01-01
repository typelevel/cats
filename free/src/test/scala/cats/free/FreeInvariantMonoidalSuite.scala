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

import cats.{Id, InvariantMonoidal}
import cats.arrow.FunctionK
import cats.kernel.Eq
import cats.instances.all.*
import cats.laws.discipline.{InvariantMonoidalTests, MiniInt, SerializableTests}
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.syntax.invariant.*
import cats.syntax.semigroupal.*
import cats.syntax.eq.*
import org.scalacheck.Prop.*
import cats.tests.BinCodecInvariantMonoidalSuite.*
import cats.tests.CatsSuite
import org.scalacheck.{Arbitrary, Gen}

class FreeInvariantMonoidalSuite extends CatsSuite {
  implicit def freeInvariantMonoidalArbitrary[F[_], A](implicit
    F: Arbitrary[F[A]],
    A: Arbitrary[A]
  ): Arbitrary[FreeInvariantMonoidal[F, A]] =
    Arbitrary(
      Gen.oneOf(A.arbitrary.map(FreeInvariantMonoidal.pure[F, A]), F.arbitrary.map(FreeInvariantMonoidal.lift[F, A]))
    )

  implicit def freeInvariantMonoidalEq[S[_]: InvariantMonoidal, A](implicit
    SA: Eq[S[A]]
  ): Eq[FreeInvariantMonoidal[S, A]] =
    Eq.by(_.foldMap(FunctionK.id))

  implicit val isoFreeBinCodec: Isomorphisms[FreeInvariantMonoidal[BinCodec, *]] =
    Isomorphisms.invariant[FreeInvariantMonoidal[BinCodec, *]]

  checkAll("FreeInvariantMonoidal[BinCodec, *]",
           InvariantMonoidalTests[FreeInvariantMonoidal[BinCodec, *]].invariantMonoidal[MiniInt, Boolean, Boolean]
  )
  checkAll("InvariantMonoidal[FreeInvariantMonoidal[BinCodec, *]]",
           SerializableTests.serializable(InvariantMonoidal[FreeInvariantMonoidal[BinCodec, *]])
  )

  test("FreeInvariantMonoidal#fold") {
    forAll { (i1: BinCodec[MiniInt]) =>
      val n = MiniInt.unsafeFromInt(2)
      val i2 = InvariantMonoidal[BinCodec].point(n)
      val iExpr = i1.product(i2.imap(_ * n)(_ / n))

      val f1 = FreeInvariantMonoidal.lift[BinCodec, MiniInt](i1)
      val f2 = FreeInvariantMonoidal.pure[BinCodec, MiniInt](n)
      val fExpr = f1.product(f2.imap(_ * n)(_ / n))

      assert(fExpr.fold === iExpr)
    }
  }

  implicit val idIsInvariantMonoidal: InvariantMonoidal[Id] = new InvariantMonoidal[Id] {
    def product[A, B](fa: Id[A], fb: Id[B]): Id[(A, B)] = fa -> fb
    def imap[A, B](fa: Id[A])(f: A => B)(g: B => A): Id[B] = f(fa)
    def unit: Id[Unit] = ()
  }

  test("FreeInvariantMonoidal#compile") {
    val x = FreeInvariantMonoidal.lift[Id, Int](1)
    val y = FreeInvariantMonoidal.pure[Id, Int](2)
    val p = x.imap(_ * 2)(_ / 2)
    val nt = FunctionK.id[Id]
    val r1 = y.product(p)
    val r2 = r1.compile(nt)
    assert(r1.foldMap(nt) === r2.foldMap(nt))
  }

  test("FreeInvariantMonoidal#analyze") {
    type G[A] = List[Int]
    val countingNT = new FunctionK[List, G] { def apply[A](la: List[A]): G[A] = List(la.length) }

    val fli1 = FreeInvariantMonoidal.lift[List, Int](List(1, 3, 5, 7))
    assert(fli1.analyze[G[Int]](countingNT) === List(4))

    val fli2 = FreeInvariantMonoidal.lift[List, Int](List.empty)
    assert(fli2.analyze[G[Int]](countingNT) === List(0))
  }
}
