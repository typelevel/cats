package cats


import cats.data._
import cats.tests.CatsSuite
import cats.laws.discipline.{ParallelTests => ParallelTypeclassTests}
import cats.laws.discipline.eq._
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary

class ParallelTests extends CatsSuite {


  test("ParTraversing Either should accumulate errors") {
    forAll { es: List[Either[String, Int]] =>
      val lefts = es.collect {
        case Left(e) => e
      }.foldMap(identity)

      (es.parSequence.fold(identity, i => Monoid[String].empty)) should === (lefts)
    }
  }

  test("ParTraverse identity should be equivalent to parSequence") {
    forAll { es: List[Either[String, Int]] =>
      (es.parTraverse(identity)) should === (es.parSequence)
    }
  }

  test("ParTraverse_ identity should be equivalent to parSequence_") {
    forAll { es: Set[Either[String, Int]] =>
      (Parallel.parTraverse_(es)(identity)) should === (Parallel.parSequence_(es))
    }
  }

  test("parAp2 accumulates errors in order") {
    val plus = (_: Int) + (_: Int)
    val rightPlus: Either[String, (Int, Int) => Int] = Right(plus)
    Parallel.parAp2(rightPlus)("Hello".asLeft, "World".asLeft) should === (Left("HelloWorld"))
  }

  checkAll("Parallel[Either[String, ?], Validated[String, ?]]", ParallelTypeclassTests[Either[String, ?], Validated[String, ?], Int].parallel)
  checkAll("Parallel[OptionT[M, ?], Nested[F, Option, ?]]", ParallelTypeclassTests[OptionT[Either[String, ?], ?], Nested[Validated[String, ?], Option, ?], Int].parallel)
  checkAll("Parallel[EitherT[M, E, ?], Nested[F, Validated[E, ?], ?]]", ParallelTypeclassTests[EitherT[Either[String, ?], String, ?], Nested[Validated[String, ?], Validated[String, ?], ?], Int].parallel)

  {
    implicit def kleisliEq[F[_], A, B](implicit A: Arbitrary[A], FB: Eq[F[B]]): Eq[Kleisli[F, A, B]] =
      Eq.by[Kleisli[F, A, B], A => F[B]](_.run)

    checkAll("Parallel[KlesliT[Id, ?], Nested[F, Option, ?]]", ParallelTypeclassTests[Kleisli[Either[String, ?], Int, ?], Kleisli[Validated[String, ?], Int, ?], Int].parallel)
  }
}
