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

import cats._
import cats.arrow.FunctionK
import cats.data._
import cats.instances.all._
import cats.laws.discipline._
import cats.syntax.applicative._
import cats.syntax.either._
import cats.tests.CatsSuite
import org.scalacheck.{Arbitrary, Cogen, Gen}
import cats.syntax.eq._
import org.scalacheck.Prop._

class FreeTSuite extends CatsSuite {

  import FreeTSuite._

  {
    implicit val freeTFlatMap: FlatMap[FreeTOption] = FreeT.catsFreeFlatMapForFreeT[Option, Option]
    checkAll("FreeT[Option, Option, Int]", FlatMapTests[FreeTOption].flatMap[Int, Int, Int])
    checkAll("FlatMap[FreeT[Option, Option, *]]", SerializableTests.serializable(FlatMap[FreeTOption]))
  }

  {
    implicit val freeTMonad: Monad[FreeTOption] = FreeT.catsFreeMonadForFreeT[Option, Option]
    checkAll("FreeT[Option, Option, Int]", MonadTests[FreeTOption].monad[Int, Int, Int])
    checkAll("Monad[FreeT[Option, Option, *]]", SerializableTests.serializable(Monad[FreeTOption]))
  }

  {
    implicit val freeTSemigroupK: SemigroupK[FreeTOption] = FreeT.catsFreeSemigroupKForFreeT[Option, Option]
    checkAll("FreeT[Option, Option, Int]", SemigroupKTests[FreeTOption].semigroupK[Int])
    checkAll("SemigroupK[FreeT[Option, Option, *]]", SerializableTests.serializable(SemigroupK[FreeTOption]))
  }

  {
    implicit val freeTAlternative: Alternative[FreeTOption] = FreeT.catsFreeAlternativeForFreeT[Option, Option]
    checkAll("FreeT[Option, Option, Int]", AlternativeTests[FreeTOption].alternative[Int, Int, Int])
    checkAll("Alternative[FreeT[Option, Option, *]]", SerializableTests.serializable(Alternative[FreeTOption]))
  }

  {
    implicit val eqEitherTFA: Eq[EitherT[FreeTOption, Unit, Int]] = EitherT.catsDataEqForEitherT[FreeTOption, Unit, Int]
    checkAll("FreeT[Option, Option, Int]", MonadErrorTests[FreeTOption, Unit].monadError[Int, Int, Int])
    checkAll("MonadError[FreeT[Option, Option, *], Unit]",
             SerializableTests.serializable(MonadError[FreeTOption, Unit])
    )
  }

  checkAll("FreeT[Option, Option, Int", DeferTests[FreeTOption].defer[Int])

  test("FlatMap stack safety tested with 50k flatMaps") {
    val expected = Applicative[FreeTOption].unit
    val result =
      Monad[FreeTOption].tailRecM(0)((i: Int) =>
        if (i < 50000)
          Applicative[FreeTOption].pure(Either.left[Int, Unit](i + 1))
        else
          Applicative[FreeTOption].pure(Either.right[Int, Unit](()))
      )

    assert(Eq[FreeTOption[Unit]].eqv(expected, result))
  }

  test("Stack safe with 50k left-associated flatMaps") {
    val expected = Applicative[FreeTOption].unit
    val result =
      (0 until 50000).foldLeft(Applicative[FreeTOption].unit)((fu, i) =>
        fu.flatMap(u => Applicative[FreeTOption].pure(u))
      )

    assert(Eq[FreeTOption[Unit]].eqv(expected, result))
  }

  test("Stack safe with flatMap followed by 50k maps") {
    val expected = Applicative[FreeTOption].unit
    val result =
      (0 until 50000).foldLeft(().pure[FreeTOption].flatMap(_.pure[FreeTOption]))((fu, i) => fu.map(identity))

    assert(Eq[FreeTOption[Unit]].eqv(expected, result))
  }

  test("mapK to universal id equivalent to original instance") {
    forAll { (a: FreeTOption[Int]) =>
      val b = a.mapK(FunctionK.id)
      assert(Eq[FreeTOption[Int]].eqv(a, b))
    }
  }

  test("mapK stack-safety") {
    val a = (0 until 50000).foldLeft(Applicative[FreeTOption].unit)((fu, i) =>
      fu.flatMap(u => Applicative[FreeTOption].pure(u))
    )
    val b = a.mapK(FunctionK.id)
  }

  test("compile to universal id equivalent to original instance") {
    forAll { (a: FreeTOption[Int]) =>
      val b = a.compile(FunctionK.id)
      assert(Eq[FreeTOption[Int]].eqv(a, b))
      val fk = FreeT.compile[Option, Option, Option](FunctionK.id)
      assert(a === fk(a))
    }
  }

  test("compile stack-safety") {
    val a = (0 until 50000).foldLeft(Applicative[FreeTOption].unit)((fu, i) =>
      fu.flatMap(u => Applicative[FreeTOption].pure(u))
    )
    val b = a.compile(FunctionK.id) // used to overflow
  }

  test("foldMap consistent with runM") {
    forAll { (a: FreeTOption[Int]) =>
      val x = a.runM(identity)
      val y = a.foldMap(FunctionK.id)
      val fk = FreeT.foldMap[Option, Option](FunctionK.id)
      assert(Eq[Option[Int]].eqv(x, y))
      assert(y === fk(a))
    }
  }

  // NB: this does not analogously cause problems for the SemigroupK implementation as semigroup's effects associate while errors do not
  test("handle errors in non-head suspensions") {
    type F[A] = FreeT[Id, Option, A]
    val F = MonadError[F, Unit]

    val eff = F.flatMap(F.unit)(_ => F.raiseError[String](()))
    assert(F.attempt(eff).runM(Some(_)) === Some(Left(())))
  }

  sealed trait Test1Algebra[A]

  case class Test1[A](value: Int, f: Int => A) extends Test1Algebra[A]

  def test1[A](value: Int, f: Int => A): Test1Algebra[A] = Test1(value, f)

  object Test1Algebra {
    implicit def test1AlgebraAFunctor: Functor[Test1Algebra] =
      new Functor[Test1Algebra] {
        def map[A, B](a: Test1Algebra[A])(f: A => B): Test1Algebra[B] =
          a match {
            case Test1(k, h) => Test1(k, x => f(h(x)))
          }
      }

    implicit def test1AlgebraArbitrary[A](implicit
      seqArb: Arbitrary[Int],
      intAArb: Arbitrary[Int => A]
    ): Arbitrary[Test1Algebra[A]] =
      Arbitrary(for { s <- seqArb.arbitrary; f <- intAArb.arbitrary } yield Test1(s, f))
  }

  sealed trait Test2Algebra[A]

  case class Test2[A](value: Int, f: Int => A) extends Test2Algebra[A]

  def test2[A](value: Int, f: Int => A): Test2Algebra[A] = Test2(value, f)

  object Test2Algebra {
    implicit def test2AlgebraAFunctor: Functor[Test2Algebra] =
      new Functor[Test2Algebra] {
        def map[A, B](a: Test2Algebra[A])(f: A => B): Test2Algebra[B] =
          a match {
            case Test2(k, h) => Test2(k, x => f(h(x)))
          }
      }

    implicit def test2AlgebraArbitrary[A](implicit
      seqArb: Arbitrary[Int],
      intAArb: Arbitrary[Int => A]
    ): Arbitrary[Test2Algebra[A]] =
      Arbitrary(for { s <- seqArb.arbitrary; f <- intAArb.arbitrary } yield Test2(s, f))
  }

  type T[A] = EitherK[Test1Algebra, Test2Algebra, A]

  object Test1Interpreter extends FunctionK[Test1Algebra, Id] {
    override def apply[A](fa: Test1Algebra[A]): Id[A] =
      fa match {
        case Test1(k, h) => h(k)
      }
  }

  object Test2Interpreter extends FunctionK[Test2Algebra, Id] {
    override def apply[A](fa: Test2Algebra[A]): Id[A] =
      fa match {
        case Test2(k, h) => h(k)
      }
  }

  val eitherKInterpreter: FunctionK[T, Id] = Test1Interpreter.or(Test2Interpreter)

  test(".liftInject") {
    forAll { (x: Int, y: Int) =>
      def res[F[_]](implicit I0: Test1Algebra :<: F, I1: Test2Algebra :<: F): FreeT[F, Id, Int] =
        for {
          a <- FreeT.liftInject[Id, F](test1(x, identity))
          b <- FreeT.liftInject[Id, F](test2(y, identity))
        } yield a + b
      assert(res[T].foldMap(eitherKInterpreter) == (x + y))
    }
  }

  test("== should not return true for unequal instances") {
    val a = FreeT.pure[List, Option, Int](1).flatMap(x => FreeT.pure(2))
    val b = FreeT.pure[List, Option, Int](3).flatMap(x => FreeT.pure(4))
    assert(a != b)
  }

  test("toString is stack-safe") {
    val result =
      (0 until 50000).foldLeft(().pure[FreeTOption].flatMap(_.pure[FreeTOption]))((fu, i) => fu.map(identity))
    assert(result.toString.length > 0)
  }

  private[free] def liftTCompilationTests() = {
    val a: Either[String, Int] = Right(42)
    val b: FreeT[Option, Either[String, *], Int] = FreeT.liftT(a)
  }

}

object FreeTSuite extends FreeTSuiteInstances {

  import Arbitrary._
  import org.scalacheck.Arbitrary

  implicit def freeTArb[F[_], G[_]: Applicative, A](implicit
    F: Arbitrary[F[A]],
    G: Arbitrary[G[A]],
    A: Arbitrary[A]
  ): Arbitrary[FreeT[F, G, A]] =
    Arbitrary(freeTGen[F, G, A](4))

  private def freeTGen[F[_], G[_]: Applicative, A](
    maxDepth: Int
  )(implicit F: Arbitrary[F[A]], G: Arbitrary[G[A]], A: Arbitrary[A]): Gen[FreeT[F, G, A]] = {
    val noFlatMapped = Gen.oneOf(
      A.arbitrary.map(FreeT.pure[F, G, A]),
      F.arbitrary.map(FreeT.liftF[F, G, A])
    )

    val nextDepth = Gen.chooseNum(1, math.max(1, maxDepth - 1))

    def withFlatMapped =
      for {
        fDepth <- nextDepth
        freeDepth <- nextDepth
        f <- arbFunction1[A, FreeT[F, G, A]](Arbitrary(freeTGen[F, G, A](fDepth)),
                                             Cogen[Unit].contramap(_ => ())
        ).arbitrary
        freeFGA <- freeTGen[F, G, A](freeDepth)
      } yield freeFGA.flatMap(f)

    if (maxDepth <= 1) noFlatMapped
    else Gen.oneOf(noFlatMapped, withFlatMapped)
  }

}

trait FreeTSuiteInstances {

  import FreeT._
  import IndexedStateT._
  import cats.tests.IndexedStateTSuite._
  import SemigroupalTests._

  type IntState[A] = State[MiniInt, A]
  type FreeTOption[A] = FreeT[Option, Option, A]
  type FreeTState[A] = FreeT[IntState, IntState, A]

  case class JustFunctor[A](a: A)

  implicit val ftlWIso: Isomorphisms[FreeTOption] = SemigroupalTests.Isomorphisms.invariant[FreeTOption]

  implicit val ftlSIso: Isomorphisms[FreeTState] = SemigroupalTests.Isomorphisms.invariant[FreeTState]

  implicit val jfFunctor: Functor[JustFunctor] = new Functor[JustFunctor] {
    override def map[A, B](fa: JustFunctor[A])(f: A => B): JustFunctor[B] = JustFunctor(f(fa.a))
  }

  implicit def freeTOptionEq[A](implicit A: Eq[A]): Eq[FreeTOption[A]] =
    Eq.by(_.runM(identity))

  implicit def freeTStateEq[A](implicit A: Eq[A]): Eq[FreeTState[A]] =
    Eq.by(_.runM(identity))
}
