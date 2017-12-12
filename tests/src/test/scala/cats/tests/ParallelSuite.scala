package cats
package tests

import cats._
import cats.data.NonEmptyList.ZipNonEmptyList
import cats.data.NonEmptyVector.ZipNonEmptyVector
import cats.data._
import org.scalatest.FunSuite
import cats.laws.discipline.{ApplicativeErrorTests, NonEmptyParallelTests, SerializableTests, ParallelTests}
import cats.laws.discipline.eq._
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary
import org.typelevel.discipline.scalatest.Discipline
import scala.collection.immutable.SortedSet

class ParallelSuite extends CatsSuite with ApplicativeErrorForEitherTest {


  test("ParSequence Either should accumulate errors") {
    forAll { es: List[Either[String, Int]] =>
      val lefts = es.collect {
        case Left(e) => e
      }.foldMap(identity)

      es.parSequence.fold(identity, i => Monoid[String].empty) should === (lefts)
    }
  }

  test("ParSequence Ior should accumulate errors") {
    forAll { es: List[Ior[String, Int]] =>
      val lefts = es.map(_.left).collect {
        case Some(e) => e
      }.foldMap(identity)
      es.parSequence.left.getOrElse(Monoid[String].empty) should === (lefts)
    }
  }

  test("ParSequence Ior should sequence values") {
    forAll { es: List[Ior[String, Int]] =>
      es.parSequence.right should === (es.map(_.toOption).sequence)
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
    val w1: WriterT[Either[String, ?], String, Int] = WriterT.liftF(Left("Too "))
    val w2: WriterT[Either[String, ?], String, Int] = WriterT.liftF(Left("bad."))

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

  test("ParMap over List should be consistent with zip") {
    forAll { (as: List[Int], bs: List[Int], cs: List[Int]) =>
      val zipped = as.zip(bs).map {
        case (a, b) => a + b
      }.zip(cs).map {
        case (a, b) => a + b
      }

      (as, bs, cs).parMapN(_ + _ + _) should === (zipped)
    }
  }

  test("ParMap over Vector should be consistent with zip") {
    forAll { (as: Vector[Int], bs: Vector[Int], cs: Vector[Int]) =>
      val zipped = as.zip(bs).map {
        case (a, b) => a + b
      }.zip(cs).map {
        case (a, b) => a + b
      }

      (as, bs, cs).parMapN(_ + _ + _) should === (zipped)
    }
  }

  test("ParMap over Stream should be consistent with zip") {
    forAll { (as: Stream[Int], bs: Stream[Int], cs: Stream[Int]) =>
      val zipped = as.zip(bs).map {
        case (a, b) => a + b
      }.zip(cs).map {
        case (a, b) => a + b
      }

      (as, bs, cs).parMapN(_ + _ + _) should === (zipped)
    }
  }

  test("IorT leverages parallel effect instances when it exists") {
    case class Marker(value: String) extends Exception("marker") {
      override def fillInStackTrace: Throwable = null
    }

    def checkMarker[A](f: => A): Option[String] =
      try { f; None } catch {
        case marker: Marker => marker.value.some
        case _: Throwable => None
      }

    final case class Effect[A](value: A)
    val monadInstance: Monad[Effect] = new Monad[Effect] {
      def pure[A](a: A): Effect[A] = Effect(a)
      def flatMap[A, B](fa: Effect[A])(f: A => Effect[B]): Effect[B] = throw Marker("sequential")
      def tailRecM[A, B](a: A)(f: A => Effect[Either[A, B]]): Effect[B] = ???
    }
    val parallelInstance: Parallel[Effect, Effect] = new Parallel[Effect, Effect] {
      def parallel: Effect ~> Effect = arrow.FunctionK.id
      def sequential: Effect ~> Effect = arrow.FunctionK.id

      def applicative: Applicative[Effect] = new Applicative[Effect] {
        def pure[A](a: A): Effect[A] = Effect(a)
        def ap[A, B](ff: Effect[A => B])(fa: Effect[A]): Effect[B] = throw Marker("parallel")
      }
      def monad: Monad[Effect] = monadInstance
    }

    val iorts: List[IorT[Effect, String, Int]] = List(
      IorT.leftT("hello")(monadInstance),
      IorT.bothT(" world", 404)(monadInstance),
      IorT.rightT(123)(monadInstance))

    val resultSansInstance = {
      implicit val ev0 = monadInstance
      checkMarker(iorts.parSequence)
    }
    val resultWithInstance = {
      implicit val ev0 = monadInstance
      implicit val ev1 = parallelInstance
      checkMarker(iorts.parSequence)
    }

    resultSansInstance should === ("sequential".some)
    resultWithInstance should === ("parallel".some)
  }

  checkAll("Parallel[Either[String, ?], Validated[String, ?]]", ParallelTests[Either[String, ?], Validated[String, ?]].parallel[Int, String])
  checkAll("Parallel[Ior[String, ?], Ior[String, ?]]", ParallelTests[Ior[String, ?], Ior[String, ?]].parallel[Int, String])
  checkAll("Parallel[IorT[F, String, ?], IorT[F, String, ?]] with parallel effect", ParallelTests[IorT[Either[String, ?], String, ?], IorT[Validated[String, ?], String, ?]].parallel[Int, String])
  checkAll("Parallel[IorT[F, String, ?], IorT[F, String, ?]] with sequential effect", ParallelTests[IorT[Option, String, ?], IorT[Option, String, ?]].parallel[Int, String])
  checkAll("Parallel[OptionT[M, ?], Nested[F, Option, ?]]", ParallelTests[OptionT[Either[String, ?], ?], Nested[Validated[String, ?], Option, ?]].parallel[Int, String])
  checkAll("Parallel[EitherT[M, String, ?], Nested[F, Validated[String, ?], ?]]", ParallelTests[EitherT[Either[String, ?], String, ?], Nested[Validated[String, ?], Validated[String, ?], ?]].parallel[Int, String])
  checkAll("Parallel[EitherT[Option, String, ?], Nested[Option, Validated[String, ?], ?]]", ParallelTests[EitherT[Option, String, ?], Nested[Option, Validated[String, ?], ?]].parallel[Int, String])
  checkAll("Parallel[WriterT[M, Int, ?], WriterT[F, Int, ?]]", ParallelTests[WriterT[Either[String, ?], Int, ?], WriterT[Validated[String, ?], Int, ?]].parallel[Int, String])
  checkAll("NonEmptyParallel[Vector, ZipVector]", NonEmptyParallelTests[Vector, ZipVector].nonEmptyParallel[Int, String])
  checkAll("NonEmptyParallel[List, ZipList]", NonEmptyParallelTests[List, ZipList].nonEmptyParallel[Int, String])
  // Can't test Parallel here, as Applicative[ZipStream].pure doesn't terminate
  checkAll("Parallel[Stream, ZipStream]", NonEmptyParallelTests[Stream, ZipStream].nonEmptyParallel[Int, String])
  checkAll("NonEmptyParallel[NonEmptyVector, ZipNonEmptyVector]", NonEmptyParallelTests[NonEmptyVector, ZipNonEmptyVector].nonEmptyParallel[Int, String])
  checkAll("NonEmptyParallel[NonEmptyList, ZipNonEmptyList]", NonEmptyParallelTests[NonEmptyList, ZipNonEmptyList].nonEmptyParallel[Int, String])
  checkAll("Parallel[NonEmptyStream, OneAnd[ZipStream, ?]", ParallelTests[NonEmptyStream, OneAnd[ZipStream, ?]].parallel[Int, String])


  checkAll("Parallel[Id, Id]", ParallelTests[Id, Id].parallel[Int, String])

  checkAll("NonEmptyParallel[NonEmptyList, ZipNonEmptyList]", SerializableTests.serializable(NonEmptyParallel[NonEmptyList, ZipNonEmptyList]))

  checkAll("Parallel[Either[String, ?], Validated[String, ?]]", SerializableTests.serializable(Parallel[Either[String, ?], Validated[String, ?]]))

  {
    implicit def kleisliEq[F[_], A, B](implicit A: Arbitrary[A], FB: Eq[F[B]]): Eq[Kleisli[F, A, B]] =
      Eq.by[Kleisli[F, A, B], A => F[B]](_.run)

    checkAll("Parallel[KlesliT[M, ?], Nested[F, Option, ?]]", ParallelTests[Kleisli[Either[String, ?], Int, ?], Kleisli[Validated[String, ?], Int, ?]].parallel[Int, String])
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
