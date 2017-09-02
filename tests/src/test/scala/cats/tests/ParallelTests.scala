package cats


import cats.data.NonEmptyList.ZipNonEmptyList
import cats.data.NonEmptyVector.ZipNonEmptyVector
import cats.data._
import cats.tests.CatsSuite
import org.scalatest.FunSuite
import cats.laws.discipline.{ApplicativeErrorTests, ParallelTests => ParallelTypeclassTests}
import cats.laws.discipline.eq._
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary
import org.typelevel.discipline.scalatest.Discipline

class ParallelTests extends CatsSuite with ApplicativeErrorForEitherTest {


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

  test("Kleisli with Either should accumulate errors") {
    val k1: Kleisli[Either[String, ?], String, Int] = Kleisli(s => Right(s.length))
    val k2: Kleisli[Either[String, ?], String, Int] = Kleisli(s => Left("Boo"))
    val k3: Kleisli[Either[String, ?], String, Int] = Kleisli(s => Left("Nope"))

    (List(k1,k2,k3).parSequence.run("Hello")) should === (Left("BooNope"))

  }

  test("WriterT with Either should accumulate errors") {
    val w1: WriterT[Either[String, ?], String, Int] = WriterT.lift(Left("Too "))
    val w2: WriterT[Either[String, ?], String, Int] = WriterT.lift(Left("bad."))

    ((w1,w2).parMapN(_ + _).value) should === (Left("Too bad."))

  }

  test("ParMap over NonEmptyList should be consistent with zip") {
    forAll { (as: NonEmptyList[Int], bs: NonEmptyList[Int], cs: NonEmptyList[Int]) =>
      (as, bs, cs).parMapN(_ + _ + _) should === (as.zipWith(bs)(_ + _).zipWith(cs)(_ + _))
    }
  }

  test("ParMap over NonEmptyVector should be consistent with zip") {
    forAll { (as: NonEmptyVector[Int], bs: NonEmptyVector[Int], cs: NonEmptyVector[Int]) =>
      (as, bs, cs).parMapN(_ + _ + _) should === (as.zipWith(bs)(_ + _).zipWith(cs)(_ + _))
    }
  }

  checkAll("Parallel[Either[String, ?], Validated[String, ?]]", ParallelTypeclassTests[Either[String, ?], Validated[String, ?], Int].parallel)
  checkAll("Parallel[OptionT[M, ?], Nested[F, Option, ?]]", ParallelTypeclassTests[OptionT[Either[String, ?], ?], Nested[Validated[String, ?], Option, ?], Int].parallel)
  checkAll("Parallel[EitherT[M, String, ?], Nested[F, Validated[String, ?], ?]]", ParallelTypeclassTests[EitherT[Either[String, ?], String, ?], Nested[Validated[String, ?], Validated[String, ?], ?], Int].parallel)
  checkAll("Parallel[WriterT[M, Int, ?], WriterT[F, Int, ?]]", ParallelTypeclassTests[WriterT[Either[String, ?], Int, ?], WriterT[Validated[String, ?], Int, ?], Int].parallel)
  checkAll("Parallel[NonEmptyList, ZipNonEmptyList]", ParallelTypeclassTests[NonEmptyList, ZipNonEmptyList, Int].parallel)
  checkAll("Parallel[NonEmptyVector, ZipNonEmptyVector]", ParallelTypeclassTests[NonEmptyVector, ZipNonEmptyVector, Int].parallel)

  {
    implicit def kleisliEq[F[_], A, B](implicit A: Arbitrary[A], FB: Eq[F[B]]): Eq[Kleisli[F, A, B]] =
      Eq.by[Kleisli[F, A, B], A => F[B]](_.run)

    checkAll("Parallel[KlesliT[M, ?], Nested[F, Option, ?]]", ParallelTypeclassTests[Kleisli[Either[String, ?], Int, ?], Kleisli[Validated[String, ?], Int, ?], Int].parallel)
  }


}

trait ApplicativeErrorForEitherTest extends FunSuite with Discipline {
  import cats.instances.either._
  import cats.instances.parallel._
  import cats.instances.string._
  import cats.instances.int._
  import cats.instances.unit._
  import cats.instances.tuple._

  implicit val parVal = Parallel.applicativeError[Validated[String, ?], Either[String, ?], String]

  implicit def eqV[A: Eq,B: Eq]: Eq[Validated[A, B]] = cats.data.Validated.catsDataEqForValidated



  checkAll("ApplicativeError[Validated[String, Int]]", ApplicativeErrorTests[Validated[String, ?], String].applicativeError[Int, Int, Int])
}
