package cats


import cats.data._
import cats.tests.CatsSuite
import org.scalatest.FunSuite
import cats.laws.discipline.{ApplicativeErrorTests, SerializableTests, ParallelTests => ParallelTypeclassTests}
import cats.laws.discipline.eq._
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary
import org.typelevel.discipline.scalatest.Discipline
import scala.collection.immutable.SortedSet

class ParallelSuite extends CatsSuite with ApplicativeErrorForEitherTest {


  test("ParTraversing Either should accumulate errors") {
    forAll { es: List[Either[String, Int]] =>
      val lefts = es.collect {
        case Left(e) => e
      }.foldMap(identity)

      es.parSequence.fold(identity, i => Monoid[String].empty) should === (lefts)
    }
  }

  test("ParTraverse identity should be equivalent to parSequence") {
    forAll { es: List[Either[String, Int]] =>
      es.parTraverse(identity) should === (es.parSequence)
    }
  }

  test("ParTraverse_ identity should be equivalent to parSequence_") {
    forAll { es: SortedSet[Either[String, Int]] =>
      Parallel.parTraverse_(es)(identity) should === (Parallel.parSequence_(es))
    }
  }

  test("ParNonEmptyTraverse identity should be equivalent to parNonEmptySequence") {
    forAll { es: NonEmptyVector[Either[String, Int]] =>
      Parallel.parNonEmptyTraverse(es)(identity) should === (Parallel.parNonEmptySequence(es))
    }
  }

  test("ParNonEmptyTraverse_ identity should be equivalent to parNonEmptySequence_") {
    forAll { es: NonEmptyList[Either[String, Int]] =>
      Parallel.parNonEmptyTraverse_(es)(identity) should === (Parallel.parNonEmptySequence_(es))
    }
  }

  test("ParFlatTraverse should be equivalent to parTraverse map flatten") {
    forAll { es: List[Either[String, Int]] =>
      val f: Int => List[Int] = i => List(i, i + 1)
      Parallel.parFlatTraverse(es)(e => e.map(f)) should
        === (es.parTraverse(e => e.map(f)).map(_.flatten))
    }
  }

  test("ParFlatTraverse identity should be equivalent to parFlatSequence") {
    forAll { es: List[Either[String, List[Int]]] =>
      Parallel.parFlatTraverse(es)(identity) should === (Parallel.parFlatSequence(es))
    }
  }

  test("ParNonEmptyFlatTraverse should be equivalent to parNonEmptyTraverse map flatten") {
    forAll { es: NonEmptyList[Either[String, Int]] =>
      val f: Int => NonEmptyList[Int] = i => NonEmptyList.of(i, i + 1)
      Parallel.parNonEmptyFlatTraverse(es)(e => e.map(f)) should
        === (Parallel.parNonEmptyTraverse(es)(e => e.map(f)).map(_.flatten))
    }
  }

  test("ParNonEmptyFlatTraverse identity should be equivalent to parNonEmptyFlatSequence") {
    forAll { es: NonEmptyList[Either[String, NonEmptyList[Int]]] =>
      Parallel.parNonEmptyFlatTraverse(es)(identity) should === (Parallel.parNonEmptyFlatSequence(es))
    }
  }

  test("parAp accumulates errors in order") {
    val right: Either[String, Int => Int] = Left("Hello")
    Parallel.parAp(right)("World".asLeft) should === (Left("HelloWorld"))
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


  checkAll("Parallel[Either[String, ?], Validated[String, ?]]", ParallelTypeclassTests[Either[String, ?], Validated[String, ?]].parallel[Int, String])
  checkAll("Parallel[OptionT[M, ?], Nested[F, Option, ?]]", ParallelTypeclassTests[OptionT[Either[String, ?], ?], Nested[Validated[String, ?], Option, ?]].parallel[Int, String])
  checkAll("Parallel[EitherT[M, String, ?], Nested[F, Validated[String, ?], ?]]", ParallelTypeclassTests[EitherT[Either[String, ?], String, ?], Nested[Validated[String, ?], Validated[String, ?], ?]].parallel[Int, String])
  checkAll("Parallel[EitherT[Option, String, ?], Nested[Option, Validated[String, ?], ?]]", ParallelTypeclassTests[EitherT[Option, String, ?], Nested[Option, Validated[String, ?], ?]].parallel[Int, String])
  checkAll("Parallel[WriterT[M, Int, ?], WriterT[F, Int, ?]]", ParallelTypeclassTests[WriterT[Either[String, ?], Int, ?], WriterT[Validated[String, ?], Int, ?]].parallel[Int, String])

  checkAll("Parallel[Id, Id]", ParallelTypeclassTests[Id, Id].parallel[Int, String])

  checkAll("Parallel[Either[String, ?], Validated[String, ?]]", SerializableTests.serializable(Parallel[Either[String, ?], Validated[String, ?]]))

  {
    implicit def kleisliEq[F[_], A, B](implicit A: Arbitrary[A], FB: Eq[F[B]]): Eq[Kleisli[F, A, B]] =
      Eq.by[Kleisli[F, A, B], A => F[B]](_.run)

    checkAll("Parallel[KlesliT[M, ?], Nested[F, Option, ?]]", ParallelTypeclassTests[Kleisli[Either[String, ?], Int, ?], Kleisli[Validated[String, ?], Int, ?]].parallel[Int, String])
  }


}

trait ApplicativeErrorForEitherTest extends FunSuite with Discipline {

  import cats.instances.either._
  import cats.instances.parallel._
  import cats.instances.string._
  import cats.instances.int._
  import cats.instances.unit._
  import cats.instances.tuple._

  implicit def eqV[A: Eq, B: Eq]: Eq[Validated[A, B]] = cats.data.Validated.catsDataEqForValidated

  {
    implicit val parVal = Parallel.applicativeError[Either[String, ?], Validated[String, ?], String]

    checkAll("ApplicativeError[Validated[String, Int]]", ApplicativeErrorTests[Validated[String, ?], String].applicativeError[Int, Int, Int])
  }
}
