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

import cats.{~>, Applicative, Apply, Id}
import cats.arrow.FunctionK
import cats.data.State
import cats.instances.all._
import cats.kernel.Eq
import cats.laws.discipline.{ApplicativeTests, SerializableTests}
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.syntax.apply._
import cats.syntax.eq._
import org.scalacheck.Prop._
import cats.tests.CatsSuite
import org.scalacheck.{Arbitrary, Gen}

class FreeApplicativeSuite extends CatsSuite {
  import FreeApplicativeSuite._

  implicit val iso: Isomorphisms[FreeApplicative[Option, *]] = Isomorphisms.invariant[FreeApplicative[Option, *]]

  checkAll("FreeApplicative[Option, *]", ApplicativeTests[FreeApplicative[Option, *]].applicative[Int, Int, Int])
  checkAll("Applicative[FreeApplicative[Option, *]]",
           SerializableTests.serializable(Applicative[FreeApplicative[Option, *]])
  )

  test("toString is stack-safe") {
    val r = FreeApplicative.pure[List, Int](333)
    val rr = (1 to 1000000).foldLeft(r)((r, _) => r.map(_ + 1))
    assert(rr.toString.length > 0)
  }

  test("fold/map is stack-safe") {
    val r = FreeApplicative.lift[List, Int](List(333))
    val rr = (1 to 70000).foldLeft(r)((r, _) => r.ap(FreeApplicative.lift[List, Int => Int](List((_: Int) + 1))))
    assert(rr.fold == List(333 + 70000))
    val rx = (1 to 70000).foldRight(r)((_, r) => r.ap(FreeApplicative.lift[List, Int => Int](List((_: Int) + 1))))
    assert(rx.fold == List(333 + 70000))
  }

  test("FreeApplicative#fold") {
    val n = 2
    val o1 = Option(1)
    val o2 = Applicative[Option].pure(n)

    val x = FreeApplicative.lift[Option, Int](o1)
    val y = FreeApplicative.pure[Option, Int](n)
    val f = x.map(i => (j: Int) => i + j)
    val r = y.ap(f)
    assert(r.fold === Apply[Option].map2(o1, o2)(_ + _))
  }

  test("FreeApplicative#compile") {
    forAll { (x: FreeApplicative[List, Int], y: FreeApplicative[List, Int], nt: List ~> List) =>
      assert(x.compile(nt).fold === x.foldMap(nt))
    }
  }

  test("FreeApplicative#flatCompile") {
    forAll { (x: FreeApplicative[Option, Int]) =>
      val nt = new FunctionK[Option, FreeApplicative[Option, *]] {
        def apply[A](a: Option[A]): FreeApplicative[Option, A] = FreeApplicative.lift(a)
      }

      assert(x.foldMap[FreeApplicative[Option, *]](nt).fold === x.flatCompile[Option](nt).fold)
    }
  }

  test("FreeApplicative#monad") {
    forAll { (x: FreeApplicative[List, Int]) =>
      assert(x.monad.foldMap(FunctionK.id) === x.fold)
    }
  }

  test("FreeApplicative#ap") {
    val x = FreeApplicative.ap[Id, Int, Int](1)(FreeApplicative.pure((_: Int) + 1))
    val y = FreeApplicative.lift[Id, Int](1).ap(FreeApplicative.pure((_: Int) + 1))
    assert(x === y)
  }

  // Ensure that syntax and implicit resolution work as expected.
  // If it compiles, it passes the "test".
  object SyntaxTests {

    // fixed by #568
    val fli1 = FreeApplicative.lift[List, Int](List(1, 3, 5, 7))
    val fli2 = FreeApplicative.lift[List, Int](List(1, 3, 5, 7))
    (fli1, fli2).mapN(_ + _)
  }

  test("FreeApplicative#analyze") {
    type G[A] = List[Int]
    val countingNT = new FunctionK[List, G] { def apply[A](la: List[A]): G[A] = List(la.length) }

    val fli1 = FreeApplicative.lift[List, Int](List(1, 3, 5, 7))
    assert(fli1.analyze[G[Int]](countingNT) === List(4))

    val fli2 = FreeApplicative.lift[List, Int](List.empty)
    assert(fli2.analyze[G[Int]](countingNT) === List(0))
  }

  test("foldMap order of effects - regression check for #799") {
    trait Foo[A] {
      def getA: A
    }
    final case class Bar(getA: Int) extends Foo[Int]
    final case class Baz(getA: Long) extends Foo[Long]

    type Dsl[A] = FreeApplicative[Foo, A]

    type Tracked[A] = State[String, A]

    val f = new FunctionK[Foo, Tracked] {
      def apply[A](fa: Foo[A]): Tracked[A] =
        State { s0 =>
          (s0 + fa.toString + ";", fa.getA)
        }
    }

    val x: Dsl[Int] = FreeApplicative.lift(Bar(3))
    val y: Dsl[Long] = FreeApplicative.lift(Baz(5L))

    val z1: Dsl[Long] = Apply[Dsl].map2(x, y)((x, y) => x.toLong + y)
    val z2: Dsl[Long] = Apply[Dsl].map2(y, x)((y, x) => x.toLong + y)

    assert(z1.foldMap(f).run("").value === (("Bar(3);Baz(5);", 8L)))
    assert(z2.foldMap(f).run("").value === (("Baz(5);Bar(3);", 8L)))
  }

  test("analyze order of effects - regression check for #799") {
    type Dsl[A] = FreeApplicative[Id, A]
    val x: Dsl[String] = FreeApplicative.lift[Id, String]("x")
    val y: Dsl[String] = FreeApplicative.lift[Id, String]("y")

    val z = Apply[Dsl].map2(x, y)((_, _) => ())

    val asString = new FunctionK[Id, λ[x => String]] { def apply[A](a: A): String = a.toString }

    assert(z.analyze(asString) === "xy")
  }
}

object FreeApplicativeSuite {
  private def freeGen[F[_], A](
    maxDepth: Int
  )(implicit F: Arbitrary[F[A]], FF: Arbitrary[(A, A) => A], A: Arbitrary[A]): Gen[FreeApplicative[F, A]] = {
    val noFlatMapped =
      Gen.oneOf(A.arbitrary.map(FreeApplicative.pure[F, A]), F.arbitrary.map(FreeApplicative.lift[F, A]))

    val nextDepth = Gen.chooseNum(1, math.max(1, maxDepth - 1))

    def withFlatMapped =
      for {
        fDepth <- nextDepth
        freeDepth <- nextDepth
        ff <- FF.arbitrary
        f <- freeGen[F, A](fDepth).map(_.map(l => (u: A) => ff(l, u)))
        freeFA <- freeGen[F, A](freeDepth)
      } yield freeFA.ap(f)

    if (maxDepth <= 1) noFlatMapped
    else Gen.oneOf(noFlatMapped, withFlatMapped)
  }

  implicit def freeArbitrary[F[_], A](implicit
    F: Arbitrary[F[A]],
    FF: Arbitrary[(A, A) => A],
    A: Arbitrary[A]
  ): Arbitrary[FreeApplicative[F, A]] =
    Arbitrary(freeGen[F, A](4))

  implicit def freeApplicativeEq[S[_]: Applicative, A](implicit SA: Eq[S[A]]): Eq[FreeApplicative[S, A]] =
    Eq.by(_.fold)

  implicit def catsLawsArbitraryForListNatTrans: Arbitrary[List ~> List] =
    Arbitrary(
      Gen.oneOf(FunctionK.id[List],
                new (List ~> List) {
                  def apply[A](fa: List[A]): List[A] =
                    fa ++ fa
                }
      )
    )

}
