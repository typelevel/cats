package cats
package free

import cats._
import cats.arrow.FunctionK
import cats.data._
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.tests.CatsSuite
import cats.instances.option._
import org.scalacheck.{Arbitrary, Gen, Cogen}

class FreeTSuite extends CatsSuite {

  import FreeTSuite._

  {
    implicit val freeTFlatMap: FlatMap[FreeTOption] = FreeT.catsFreeFlatMapForFreeT[Option, Option]
    checkAll("FreeT[Option, Option, Int]", FlatMapTests[FreeTOption].flatMap[Int, Int, Int])
    checkAll("FlatMap[FreeT[Option, Option, ?]]", SerializableTests.serializable(FlatMap[FreeTOption]))
  }

  {
    implicit val freeTMonad: Monad[FreeTOption] = FreeT.catsFreeMonadForFreeT[Option, Option]
    checkAll("FreeT[Option, Option, Int]", MonadTests[FreeTOption].monad[Int, Int, Int])
    checkAll("Monad[FreeT[Option, Option, ?]]", SerializableTests.serializable(Monad[FreeTOption]))
  }

  {
    implicit val freeTSemigroupK: SemigroupK[FreeTOption] = FreeT.catsFreeSemigroupKForFreeT[Option, Option]
    checkAll("FreeT[Option, Option, Int]", SemigroupKTests[FreeTOption].semigroupK[Int])
    checkAll("SemigroupK[FreeT[Option, Option, ?]]", SerializableTests.serializable(SemigroupK[FreeTOption]))
  }

  {
    implicit val freeTAlternative: Alternative[FreeTOption] = FreeT.catsFreeAlternativeForFreeT[Option, Option]
    checkAll("FreeT[Option, Option, Int]", AlternativeTests[FreeTOption].alternative[Int, Int, Int])
    checkAll("Alternative[FreeT[Option, Option, ?]]", SerializableTests.serializable(Alternative[FreeTOption]))
  }

  {
    implicit val eqEitherTFA: Eq[EitherT[FreeTOption, Unit, Int]] = EitherT.catsDataEqForEitherT[FreeTOption, Unit, Int]
    checkAll("FreeT[Option, Option, Int]", MonadErrorTests[FreeTOption, Unit].monadError[Int, Int, Int])
    checkAll("MonadError[FreeT[Option, Option, ?], Unit]", SerializableTests.serializable(MonadError[FreeTOption, Unit]))
  }

  test("FlatMap stack safety tested with 50k flatMaps") {
    val expected = Applicative[FreeTOption].pure(())
    val result =
      Monad[FreeTOption].tailRecM(0)((i: Int) =>
        if (i < 50000)
          Applicative[FreeTOption].pure(Either.left[Int, Unit](i + 1))
        else
          Applicative[FreeTOption].pure(Either.right[Int, Unit](())))

    Eq[FreeTOption[Unit]].eqv(expected, result) should ===(true)
  }

  test("Stack safe with 50k left-associated flatMaps") {
    val expected = Applicative[FreeTOption].pure(())
    val result =
      (0 until 50000).foldLeft(Applicative[FreeTOption].pure(()))(
        (fu, i) => fu.flatMap(u => Applicative[FreeTOption].pure(u))
      )

    Eq[FreeTOption[Unit]].eqv(expected, result) should ===(true)
  }

  test("Stack safe with flatMap followed by 50k maps") {
    val expected = Applicative[FreeTOption].pure(())
    val result =
      (0 until 50000).foldLeft(().pure[FreeTOption].flatMap(_.pure[FreeTOption]))(
        (fu, i) => fu.map(identity)
      )

    Eq[FreeTOption[Unit]].eqv(expected, result) should ===(true)
  }

  test("mapK to universal id equivalent to original instance") {
    forAll { a: FreeTOption[Int] =>
      val b = a.mapK(FunctionK.id)
      Eq[FreeTOption[Int]].eqv(a, b) should ===(true)
    }
  }

  test("mapK stack-safety") {
    val a = (0 until 50000).foldLeft(Applicative[FreeTOption].pure(()))(
      (fu, i) => fu.flatMap(u => Applicative[FreeTOption].pure(u))
    )
    val b = a.mapK(FunctionK.id)
  }

  test("compile to universal id equivalent to original instance") {
    forAll { a: FreeTOption[Int] =>
      val b = a.compile(FunctionK.id)
      Eq[FreeTOption[Int]].eqv(a, b) should ===(true)
      val fk = FreeT.compile[Option, Option, Option](FunctionK.id)
      a should === (fk(a))
    }
  }

  test("compile stack-safety") {
    val a = (0 until 50000).foldLeft(Applicative[FreeTOption].pure(()))(
      (fu, i) => fu.flatMap(u => Applicative[FreeTOption].pure(u))
    )
    val b = a.compile(FunctionK.id) // used to overflow
  }

  test("foldMap consistent with runM") {
    forAll { a: FreeTOption[Int] =>
      val x = a.runM(identity)
      val y = a.foldMap(FunctionK.id)
      val fk = FreeT.foldMap[Option, Option](FunctionK.id)
      Eq[Option[Int]].eqv(x, y) should ===(true)
      y should === (fk(a))
    }
  }

  test("== should not return true for unequal instances") {
    val a = FreeT.pure[List, Option, Int](1).flatMap(x => FreeT.pure(2))
    val b = FreeT.pure[List, Option, Int](3).flatMap(x => FreeT.pure(4))
    a == b should be(false)
  }

  test("toString is stack-safe") {
    val result =
      (0 until 50000).foldLeft(().pure[FreeTOption].flatMap(_.pure[FreeTOption]))(
        (fu, i) => fu.map(identity)
      )
    result.toString.length should be > 0
  }

  private[free] def liftTCompilationTests() = {
    val a: Either[String, Int]= Right(42)
    val b: FreeT[Option, Either[String, ?], Int] = FreeT.liftT(a)
  }

}

object FreeTSuite extends FreeTSuiteInstances {

  import Arbitrary._
  import org.scalacheck.Arbitrary


  implicit def freeTArb[F[_], G[_]: Applicative, A](implicit F: Arbitrary[F[A]], G: Arbitrary[G[A]], A: Arbitrary[A]): Arbitrary[FreeT[F, G, A]] =
    Arbitrary(freeTGen[F, G, A](4))

  private def freeTGen[F[_], G[_]: Applicative, A](maxDepth: Int)(implicit F: Arbitrary[F[A]], G: Arbitrary[G[A]], A: Arbitrary[A]): Gen[FreeT[F, G, A]] = {
    val noFlatMapped = Gen.oneOf(
      A.arbitrary.map(FreeT.pure[F, G, A]),
      F.arbitrary.map(FreeT.liftF[F, G, A])
    )

    val nextDepth = Gen.chooseNum(1, math.max(1, maxDepth - 1))

    def withFlatMapped = for {
      fDepth <- nextDepth
      freeDepth <- nextDepth
      f <- arbFunction1[A, FreeT[F, G, A]](Arbitrary(freeTGen[F, G, A](fDepth)), Cogen[Unit].contramap(_ => ())).arbitrary
      freeFGA <- freeTGen[F, G, A](freeDepth)
    } yield freeFGA.flatMap(f)

    if (maxDepth <= 1) noFlatMapped
    else Gen.oneOf(noFlatMapped, withFlatMapped)
  }

}

trait FreeTSuiteInstances {

  import FreeT._
  import IndexedStateT._
  import cats.kernel.instances.option._
  import cats.tests.IndexedStateTSuite._
  import SemigroupalTests._

  type IntState[A] = State[Int, A]
  type FreeTOption[A] = FreeT[Option, Option, A]
  type FreeTState[A] = FreeT[IntState, IntState, A]

  case class JustFunctor[A](a: A)

  implicit val ftlWIso: Isomorphisms[FreeTOption] = SemigroupalTests.Isomorphisms.invariant[FreeTOption]

  implicit val ftlSIso: Isomorphisms[FreeTState] = SemigroupalTests.Isomorphisms.invariant[FreeTState]

  implicit val jfFunctor: Functor[JustFunctor] = new Functor[JustFunctor] {
    override def map[A, B](fa: JustFunctor[A])(f: A => B): JustFunctor[B] = JustFunctor(f(fa.a))
  }

  implicit val intEq: Eq[Int] = new Eq[Int] {
    def eqv(a: Int, b: Int) = a == b
  }

  implicit def evalEq[A: Eq]: Eq[Eval[A]] = Eval.catsEqForEval[A]

  implicit def intStateEq[A: Eq]: Eq[IntState[A]] = stateEq[Int, A]

  implicit def intStateArb[A: Arbitrary]: Arbitrary[IntState[A]] = catsLawArbitraryForState[Int, A]

  implicit def freeTOptionEq[A](implicit A: Eq[A], OM: Monad[Option]): Eq[FreeTOption[A]] = new Eq[FreeTOption[A]] {
    def eqv(a: FreeTOption[A], b: FreeTOption[A]) = Eq[Option[A]].eqv(a.runM(identity), b.runM(identity))
  }

  implicit def freeTStateEq[A](implicit A: Eq[A], SM: Monad[IntState]): Eq[FreeTState[A]] = new Eq[FreeTState[A]] {
    def eqv(a: FreeTState[A], b: FreeTState[A]) = Eq[IntState[A]].eqv(a.runM(identity)(SM, SM), b.runM(identity)(SM, SM))
  }
}
