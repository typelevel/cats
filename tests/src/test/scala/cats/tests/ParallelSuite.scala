package cats.tests

import cats._
import cats.data._
import cats.data.NonEmptyList.ZipNonEmptyList
import cats.kernel.compat.scalaVersionSpecific._
import cats.laws.discipline.{ApplicativeErrorTests, MiniInt, NonEmptyParallelTests, ParallelTests, SerializableTests}
import cats.laws.discipline.eq._
import cats.laws.discipline.arbitrary._
import cats.syntax.bifunctor._
import cats.syntax.bitraverse._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.parallel._
import cats.syntax.traverse._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import scala.collection.immutable.SortedSet

@suppressUnusedImportWarningForScalaVersionSpecific
class ParallelSuite extends CatsSuite with ApplicativeErrorForEitherTest with ScalaVersionSpecificParallelSuite {

  test("ParSequence Either should accumulate errors") {
    forAll { (es: List[Either[String, Int]]) =>
      val lefts = es
        .collect {
          case Left(e) => e
        }
        .foldMap(identity)

      es.parSequence.fold(identity, i => Monoid[String].empty) should ===(lefts)
    }
  }

  test("ParSequence Ior should accumulate errors") {
    forAll { (es: List[Ior[String, Int]]) =>
      val lefts = es
        .map(_.left)
        .collect {
          case Some(e) => e
        }
        .foldMap(identity)
      es.parSequence.left.getOrElse(Monoid[String].empty) should ===(lefts)
    }
  }

  test("ParSequence Ior should sequence values") {
    forAll { (es: List[Ior[String, Int]]) =>
      es.parSequence.right should ===(es.map(_.toOption).sequence)
    }
  }

  test("ParTraverse identity should be equivalent to parSequence") {
    forAll { (es: List[Either[String, Int]]) =>
      es.parTraverse(identity) should ===(es.parSequence)
    }
  }

  test("ParTraverse_ identity should be equivalent to parSequence_") {
    forAll { (es: SortedSet[Either[String, Int]]) =>
      Parallel.parTraverse_(es)(identity) should ===(Parallel.parSequence_(es))
    }
  }

  test("ParTraverse_ syntax should be equivalent to Parallel.parTraverse_") {
    forAll { (es: SortedSet[Either[String, Int]]) =>
      Parallel.parTraverse_(es)(identity) should ===(es.parTraverse_(identity))
    }
  }

  test("ParSequence_ syntax should be equivalent to Parallel.parSequence_") {
    forAll { (es: SortedSet[Either[String, Int]]) =>
      Parallel.parSequence_(es) should ===(es.parSequence_)
    }
  }

  test("ParNonEmptyTraverse identity should be equivalent to parNonEmptySequence") {
    forAll { (es: NonEmptyVector[Either[String, Int]]) =>
      Parallel.parNonEmptyTraverse(es)(identity) should ===(Parallel.parNonEmptySequence(es))
    }
  }

  test("ParNonEmptyTraverse_ identity should be equivalent to parNonEmptySequence_") {
    forAll { (es: NonEmptyList[Either[String, Int]]) =>
      Parallel.parNonEmptyTraverse_(es)(identity) should ===(Parallel.parNonEmptySequence_(es))
    }
  }

  type ListTuple2[A, B] = List[(A, B)]
  implicit val catsBitraverseForListTuple2: Bitraverse[ListTuple2] = new Bitraverse[ListTuple2] {
    def bifoldLeft[A, B, C](fab: ListTuple2[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
      fab.foldLeft(c) { case (c, (a, b)) => g(f(c, a), b) }
    def bifoldRight[A, B, C](fab: ListTuple2[A, B],
                             lc: Eval[C]
    )(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] = {
      def loop(abs: ListTuple2[A, B]): Eval[C] =
        abs match {
          case Nil         => lc
          case (a, b) :: t => f(a, g(b, Eval.defer(loop(t))))
        }
      Eval.defer(loop(fab))
    }
    def bitraverse[G[_], A, B, C, D](
      fab: ListTuple2[A, B]
    )(f: A => G[C], g: B => G[D])(implicit G: Applicative[G]): G[ListTuple2[C, D]] = {
      def loop(abs: ListTuple2[A, B]): Eval[G[ListTuple2[C, D]]] =
        abs match {
          case Nil         => Now(G.pure(List.empty))
          case (a, b) :: t => G.map2Eval(G.product(f(a), g(b)), Eval.defer(loop(t)))(_ :: _)
        }
      loop(fab).value
    }
  }

  test("ParBisequence Either should accumulate errors") {
    forAll { (es: ListTuple2[Either[String, Int], Either[String, Int]]) =>
      val lefts = es
        .flatMap {
          case (a, b) => List(a, b)
        }
        .collect {
          case Left(e) => e
        }
        .foldMap(identity)

      es.parBisequence.fold(identity, i => Monoid[String].empty) should ===(lefts)
    }
  }

  test("ParBisequence Ior should accumulate errors") {
    forAll { (es: ListTuple2[Ior[String, Int], Ior[String, Int]]) =>
      val lefts = es
        .flatMap {
          case (a, b) => List(a, b)
        }
        .map(_.left)
        .collect {
          case Some(e) => e
        }
        .foldMap(identity)

      es.parBisequence.left.getOrElse(Monoid[String].empty) should ===(lefts)
    }
  }

  test("ParBisequence Ior should bisequence values") {
    forAll { (es: ListTuple2[Ior[String, Int], Ior[String, Int]]) =>
      es.parBisequence.right should ===(es.bimap(_.toOption, _.toOption).bisequence)
    }
  }

  test("ParBitraverse identity should be equivalent to parBisequence") {
    forAll { (es: (Either[String, Int], Either[String, Long])) =>
      es.parBitraverse(identity, identity) should ===(es.parBisequence)
    }
  }

  test("ParLeftSequence Either should accumulate errors") {
    forAll { (es: ListTuple2[Either[String, Int], Int]) =>
      val lefts = es
        .collect {
          case (Left(e), _) => e
        }
        .foldMap(identity)

      es.parLeftSequence.fold(identity, i => Monoid[String].empty) should ===(lefts)
    }
  }

  test("ParLeftSequence Ior should accumulate errors") {
    forAll { (es: ListTuple2[Ior[String, Int], Int]) =>
      val lefts = es
        .map {
          case (a, b) => a.left
        }
        .collect {
          case Some(e) => e
        }
        .foldMap(identity)

      es.parLeftSequence.left.getOrElse(Monoid[String].empty) should ===(lefts)
    }
  }

  test("ParLeftSequence Ior should leftSequence values") {
    forAll { (es: ListTuple2[Ior[String, Int], Int]) =>
      es.parLeftSequence.right should ===(es.bimap(_.toOption, identity).leftSequence)
    }
  }

  test("ParLeftTraverse identity should be equivalent to parLeftSequence") {
    forAll { (es: (Either[String, Int], Either[String, Long])) =>
      es.parLeftTraverse(identity) should ===(es.parLeftSequence)
    }
  }

  test("ParFlatTraverse should be equivalent to parTraverse map flatten") {
    forAll { (es: List[Either[String, Int]]) =>
      val f: Int => List[Int] = i => List(i, i + 1)
      Parallel.parFlatTraverse(es)(e => e.map(f)) should
        ===(es.parTraverse(e => e.map(f)).map(_.flatten))
    }
  }

  test("ParFlatTraverse identity should be equivalent to parFlatSequence") {
    forAll { (es: List[Either[String, List[Int]]]) =>
      Parallel.parFlatTraverse(es)(identity) should ===(Parallel.parFlatSequence(es))
    }
  }

  test("ParFlatSequence syntax should be equivalent to Parallel.parFlatSequence") {
    forAll { (es: List[Either[String, List[Int]]]) =>
      es.parFlatSequence should ===(Parallel.parFlatSequence(es))
    }
  }

  test("ParFlatTraverse syntax should be equivalent to Parallel.parFlatTraverse") {
    forAll { (es: List[Either[String, Int]]) =>
      val f: Int => List[Int] = i => List(i, i + 1)
      Parallel.parFlatTraverse(es)(e => e.map(f)) should
        ===(es.parFlatTraverse(e => e.map(f)))
    }
  }

  test("ParNonEmptyFlatTraverse should be equivalent to parNonEmptyTraverse map flatten") {
    forAll { (es: NonEmptyList[Either[String, Int]]) =>
      val f: Int => NonEmptyList[Int] = i => NonEmptyList.of(i, i + 1)
      Parallel.parNonEmptyFlatTraverse(es)(e => e.map(f)) should
        ===(Parallel.parNonEmptyTraverse(es)(e => e.map(f)).map(_.flatten))
    }
  }

  test("ParNonEmptyFlatTraverse identity should be equivalent to parNonEmptyFlatSequence") {
    forAll { (es: NonEmptyList[Either[String, NonEmptyList[Int]]]) =>
      Parallel.parNonEmptyFlatTraverse(es)(identity) should ===(Parallel.parNonEmptyFlatSequence(es))
    }
  }

  test("ParFoldMapA should be equivalent to parTraverse map combineAll (where it exists)") {
    forAll { (es: List[Int], f: Int => Either[String, String]) =>
      Parallel.parFoldMapA(es)(f) should ===(
        Parallel.parTraverse(es)(f).map(_.combineAll)
      )
    }
  }

  test("parAp accumulates errors in order") {
    val right: Either[String, Int => Int] = Left("Hello")
    Parallel.parAp(right)("World".asLeft) should ===(Left("HelloWorld"))
  }

  test("parAp2 accumulates errors in order") {
    val plus = (_: Int) + (_: Int)
    val rightPlus: Either[String, (Int, Int) => Int] = Right(plus)
    Parallel.parAp2(rightPlus)("Hello".asLeft, "World".asLeft) should ===(Left("HelloWorld"))
  }

  test("Kleisli with Either should accumulate errors") {
    val k1: Kleisli[Either[String, *], String, Int] = Kleisli(s => Right(s.length))
    val k2: Kleisli[Either[String, *], String, Int] = Kleisli(s => Left("Boo"))
    val k3: Kleisli[Either[String, *], String, Int] = Kleisli(s => Left("Nope"))

    (List(k1, k2, k3).parSequence.run("Hello")) should ===(Left("BooNope"))

  }

  test("WriterT with Either should accumulate errors") {
    val w1: WriterT[Either[String, *], String, Int] = WriterT.liftF(Left("Too "))
    val w2: WriterT[Either[String, *], String, Int] = WriterT.liftF(Left("bad."))

    ((w1, w2).parMapN(_ + _).value) should ===(Left("Too bad."))

  }

  test("ParMap over NonEmptyList should be consistent with zip") {
    forAll { (as: NonEmptyList[Int], bs: NonEmptyList[Int], cs: NonEmptyList[Int]) =>
      (as, bs, cs).parMapN(_ + _ + _) should ===(as.zipWith(bs)(_ + _).zipWith(cs)(_ + _))
    }
  }

  test("ParMap over NonEmptyVector should be consistent with zip") {
    forAll { (as: NonEmptyVector[Int], bs: NonEmptyVector[Int], cs: NonEmptyVector[Int]) =>
      (as, bs, cs).parMapN(_ + _ + _) should ===(as.zipWith(bs)(_ + _).zipWith(cs)(_ + _))
    }
  }

  test("ParMap over List should be consistent with zip") {
    forAll { (as: List[Int], bs: List[Int], cs: List[Int]) =>
      val zipped = as
        .zip(bs)
        .map {
          case (a, b) => a + b
        }
        .zip(cs)
        .map {
          case (a, b) => a + b
        }

      (as, bs, cs).parMapN(_ + _ + _) should ===(zipped)
    }
  }

  test("ParMap over Vector should be consistent with zip") {
    forAll { (as: Vector[Int], bs: Vector[Int], cs: Vector[Int]) =>
      val zipped = as
        .zip(bs)
        .map {
          case (a, b) => a + b
        }
        .zip(cs)
        .map {
          case (a, b) => a + b
        }

      (as, bs, cs).parMapN(_ + _ + _) should ===(zipped)
    }
  }

  test("ParMap over Stream should be consistent with zip") {
    forAll { (as: Stream[Int], bs: Stream[Int], cs: Stream[Int]) =>
      val zipped = as
        .zip(bs)
        .map {
          case (a, b) => a + b
        }
        .zip(cs)
        .map {
          case (a, b) => a + b
        }

      (as, bs, cs).parMapN(_ + _ + _) should ===(zipped)
    }
  }

  test("ParTupled of NonEmptyList should be consistent with ParMap of Tuple.apply") {
    forAll { (fa: NonEmptyList[Int], fb: NonEmptyList[Int], fc: NonEmptyList[Int], fd: NonEmptyList[Int]) =>
      (fa, fb, fc, fd).parTupled should ===((fa, fb, fc, fd).parMapN(Tuple4.apply))
    }
  }

  test("ParTupled of NonEmptyVector should be consistent with ParMap of Tuple.apply") {
    forAll { (fa: NonEmptyVector[Int], fb: NonEmptyVector[Int], fc: NonEmptyVector[Int], fd: NonEmptyVector[Int]) =>
      (fa, fb, fc, fd).parTupled should ===((fa, fb, fc, fd).parMapN(Tuple4.apply))
    }
  }

  test("ParTupled of List should be consistent with ParMap of Tuple.apply") {
    forAll { (fa: List[Int], fb: List[Int], fc: List[Int], fd: List[Int]) =>
      (fa, fb, fc, fd).parTupled should ===((fa, fb, fc, fd).parMapN(Tuple4.apply))
    }
  }

  test("ParTupled of Vector should be consistent with ParMap of Tuple.apply") {
    forAll { (fa: Vector[Int], fb: Vector[Int], fc: Vector[Int], fd: Vector[Int]) =>
      (fa, fb, fc, fd).parTupled should ===((fa, fb, fc, fd).parMapN(Tuple4.apply))
    }
  }

  test("ParTupled of Stream should be consistent with ParMap of Tuple.apply") {
    forAll { (fa: Stream[Int], fb: Stream[Int], fc: Stream[Int], fd: Stream[Int]) =>
      (fa, fb, fc, fd).parTupled should ===((fa, fb, fc, fd).parMapN(Tuple4.apply))
    }
  }

  test("ParTupled of List should be consistent with zip") {
    forAll { (fa: List[Int], fb: List[Int], fc: List[Int], fd: List[Int]) =>
      (fa, fb, fc, fd).parTupled should ===(fa.zip(fb).zip(fc).zip(fd).map { case (((a, b), c), d) => (a, b, c, d) })
    }
  }

  test("ParTupled of Vector should be consistent with zip") {
    forAll { (fa: Vector[Int], fb: Vector[Int], fc: Vector[Int], fd: Vector[Int]) =>
      (fa, fb, fc, fd).parTupled should ===(fa.zip(fb).zip(fc).zip(fd).map { case (((a, b), c), d) => (a, b, c, d) })
    }
  }

  test("ParTupled of Stream should be consistent with zip") {
    forAll { (fa: Stream[Int], fb: Stream[Int], fc: Stream[Int], fd: Stream[Int]) =>
      (fa, fb, fc, fd).parTupled should ===(fa.zip(fb).zip(fc).zip(fd).map { case (((a, b), c), d) => (a, b, c, d) })
    }
  }

  test("IorT leverages parallel effect instances when it exists") {
    case class Marker(value: String) extends Exception("marker") {
      override def fillInStackTrace: Throwable = null
    }

    def checkMarker[A](f: => A): Option[String] =
      try {
        f; None
      } catch {
        case marker: Marker => marker.value.some
        case _: Throwable   => None
      }

    final case class Effect[A](value: A)
    val monadInstance: Monad[Effect] = new Monad[Effect] {
      def pure[A](a: A): Effect[A] = Effect(a)
      def flatMap[A, B](fa: Effect[A])(f: A => Effect[B]): Effect[B] = throw Marker("sequential")
      def tailRecM[A, B](a: A)(f: A => Effect[Either[A, B]]): Effect[B] = ???
    }
    val parallelInstance: Parallel.Aux[Effect, Effect] = new Parallel[Effect] {
      type F[x] = Effect[x]
      def parallel: Effect ~> Effect = arrow.FunctionK.id
      def sequential: Effect ~> Effect = arrow.FunctionK.id

      def applicative: Applicative[Effect] =
        new Applicative[Effect] {
          def pure[A](a: A): Effect[A] = Effect(a)
          def ap[A, B](ff: Effect[A => B])(fa: Effect[A]): Effect[B] = throw Marker("parallel")
        }
      def monad: Monad[Effect] = monadInstance
    }

    val iorts: List[IorT[Effect, String, Int]] = List(IorT.leftT("hello")(monadInstance),
                                                      IorT.bothT(" world", 404)(monadInstance),
                                                      IorT.rightT(123)(monadInstance)
    )

    val resultSansInstance = {
      implicit val ev0: Monad[Effect] = monadInstance
      checkMarker(iorts.parSequence)
    }
    val resultWithInstance = {
      implicit val ev0: Monad[Effect] = monadInstance
      implicit val ev1: Parallel.Aux[Effect, Effect] = parallelInstance
      checkMarker(iorts.parSequence)
    }

    resultSansInstance should ===("sequential".some)
    resultWithInstance should ===("parallel".some)
  }

  checkAll("Parallel[Either[String, *]", ParallelTests[Either[String, *]].parallel[Int, String])
  checkAll("Parallel[Ior[String, *]]", ParallelTests[Ior[String, *]].parallel[Int, String])
  checkAll(
    "Parallel[IorT[F, String, *]] with parallel effect",
    ParallelTests[IorT[Either[String, *], String, *]].parallel[Int, String]
  )
  checkAll(
    "Parallel[IorT[F, String, *]] with sequential effect",
    ParallelTests[IorT[Option, String, *]].parallel[Int, String]
  )
  checkAll("Parallel[OptionT[M, *]]", ParallelTests[OptionT[Either[String, *], *]].parallel[Int, String])
  checkAll(
    "Parallel[EitherT[M, String, *]]",
    ParallelTests[EitherT[Either[String, *], String, *]]
      .parallel[Int, String]
  )
  checkAll(
    "Parallel[EitherT[Option, String, *]]",
    ParallelTests[EitherT[Option, String, *]].parallel[Int, String]
  )
  checkAll(
    "Parallel[WriterT[M, Int, *]]",
    ParallelTests[WriterT[Either[String, *], Int, *]].parallel[Int, String]
  )
  checkAll("NonEmptyParallel[Vector]", NonEmptyParallelTests[Vector].nonEmptyParallel[Int, String])
  checkAll("NonEmptyParallel[List]", NonEmptyParallelTests[List].nonEmptyParallel[Int, String])
  // Can't test Parallel here, as Applicative[ZipStream].pure doesn't terminate
  checkAll("Parallel[Stream]", NonEmptyParallelTests[Stream].nonEmptyParallel[Int, String])

  checkAll("NonEmptyParallel[NonEmptyVector]", NonEmptyParallelTests[NonEmptyVector].nonEmptyParallel[Int, String])

  checkAll("NonEmptyParallel[NonEmptyList]", NonEmptyParallelTests[NonEmptyList].nonEmptyParallel[Int, String])

  // TODO this doesn't infer?
  checkAll("Parallel[NonEmptyStream]", ParallelTests[NonEmptyStream, OneAnd[ZipStream, *]].parallel[Int, String])

  checkAll("Parallel[Id]", ParallelTests[Id].parallel[Int, String])

  checkAll("NonEmptyParallel[NonEmptyList]", SerializableTests.serializable(NonEmptyParallel[NonEmptyList]))

  checkAll("Parallel[Either[String, *]]", SerializableTests.serializable(Parallel[Either[String, *]]))

  {
    implicit def kleisliEq[F[_], A, B](implicit ev: Eq[A => F[B]]): Eq[Kleisli[F, A, B]] =
      Eq.by[Kleisli[F, A, B], A => F[B]](_.run)

    checkAll(
      "Parallel[KleisliT[M, A, *]]",
      ParallelTests[Kleisli[Either[String, *], MiniInt, *]]
        .parallel[Int, String]
    )
  }

  test("NonEmptyParallel.apply should return an appropriately typed instance given both type parameters") {
    val p1: NonEmptyParallel.Aux[Either[String, *], Validated[String, *]] =
      NonEmptyParallel[Either[String, *], Validated[String, *]]
    val p2: NonEmptyParallel.Aux[NonEmptyList, ZipNonEmptyList] = NonEmptyParallel[NonEmptyList, ZipNonEmptyList]
  }

  test("NonEmptyParallel.apply should return an appropriately typed instance given the first type parameter") {
    val p1: NonEmptyParallel.Aux[Either[String, *], Validated[String, *]] = NonEmptyParallel[Either[String, *]]
    val p2: NonEmptyParallel.Aux[NonEmptyList, ZipNonEmptyList] = NonEmptyParallel[NonEmptyList]
  }

  test("Parallel.apply should return an appropriately typed instance given both type parameters") {
    val p1: Parallel.Aux[Either[String, *], Validated[String, *]] = Parallel[Either[String, *], Validated[String, *]]
    val p2: Parallel.Aux[Stream, ZipStream] = Parallel[Stream, ZipStream]
  }

  test("Parallel.apply should return an appropriately typed instance given the first type parameter") {
    val p1: Parallel.Aux[Either[String, *], Validated[String, *]] = Parallel[Either[String, *], Validated[String, *]]
    val p2: Parallel.Aux[Stream, ZipStream] = Parallel[Stream]
  }
}

trait ApplicativeErrorForEitherTest extends AnyFunSuiteLike with FunSuiteDiscipline with Checkers {
  implicit def eqV[A: Eq, B: Eq]: Eq[Validated[A, B]] = cats.data.Validated.catsDataEqForValidated

  {
    implicit val parVal: ApplicativeError[Validated[String, *], String] =
      Parallel.applicativeError[Either[String, *], String]

    checkAll("ApplicativeError[Validated[String, Int]]",
             ApplicativeErrorTests[Validated[String, *], String].applicativeError[Int, Int, Int]
    )
  }
}
