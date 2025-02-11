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

package cats.tests

import cats.*
import cats.data.NonEmptyList.ZipNonEmptyList
import cats.data.*
import cats.kernel.compat.scalaVersionSpecific.*
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import cats.syntax.all.*
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.*

import scala.collection.immutable.SortedSet

@suppressUnusedImportWarningForScalaVersionSpecific
class ParallelSuite
    extends CatsSuite
    with ApplicativeErrorForEitherTest
    with ScalaVersionSpecificParallelSuite
    with ParallelSuiteStreamSpecific {

  test("ParSequence Either should accumulate errors") {
    forAll { (es: List[Either[String, Int]]) =>
      val lefts = es
        .collect { case Left(e) =>
          e
        }
        .foldMap(identity)

      assert(es.parSequence.fold(identity, i => Monoid[String].empty) === lefts)
    }
  }

  test("ParSequence Ior should accumulate errors") {
    forAll { (es: List[Ior[String, Int]]) =>
      val lefts = es
        .map(_.left)
        .collect { case Some(e) =>
          e
        }
        .foldMap(identity)
      assert(es.parSequence.left.getOrElse(Monoid[String].empty) === lefts)
    }
  }

  test("ParSequence Ior should sequence values") {
    forAll { (es: List[Ior[String, Int]]) =>
      assert(es.parSequence.right === (es.map(_.toOption).sequence))
    }
  }

  test("ParTraverse identity should be equivalent to parSequence") {
    forAll { (es: List[Either[String, Int]]) =>
      assert(es.parTraverse(identity) === (es.parSequence))
    }
  }

  test("ParTraverseVoid identity should be equivalent to parSequenceVoid") {
    forAll { (es: SortedSet[Either[String, Int]]) =>
      assert(Parallel.parTraverseVoid(es)(identity) === Parallel.parSequenceVoid[SortedSet, Either[String, *], Int](es))
    }
  }

  test("ParTraverseVoid syntax should be equivalent to Parallel.parTraverseVoid") {
    forAll { (es: SortedSet[Either[String, Int]]) =>
      assert(
        Parallel.parTraverseVoid[SortedSet, Either[String, *], Either[String, Int], Int](es)(identity) ===
          es.parTraverseVoid(identity)
      )
    }
  }

  test("ParSequenceVoid syntax should be equivalent to Parallel.parSequenceVoid") {
    forAll { (es: SortedSet[Either[String, Int]]) =>
      assert(Parallel.parSequenceVoid[SortedSet, Either[String, *], Int](es) === es.parSequenceVoid)
    }
  }

  test("ParNonEmptyTraverse identity should be equivalent to parNonEmptySequence") {
    forAll { (es: NonEmptyVector[Either[String, Int]]) =>
      assert(Parallel.parNonEmptyTraverse(es)(identity) === (Parallel.parNonEmptySequence(es)))
    }
  }

  test("ParNonEmptyTraverseVoid identity should be equivalent to parNonEmptySequenceVoid") {
    forAll { (es: NonEmptyList[Either[String, Int]]) =>
      assert(Parallel.parNonEmptyTraverseVoid(es)(identity) === Parallel.parNonEmptySequenceVoid(es))
    }
  }

  case class ListTuple2[A, B](value: List[(A, B)])
  implicit val catsBitraverseForListTuple2: Bitraverse[ListTuple2] = new Bitraverse[ListTuple2] {
    def bifoldLeft[A, B, C](fab: ListTuple2[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
      fab.value.foldLeft(c) { case (c, (a, b)) => g(f(c, a), b) }
    def bifoldRight[A, B, C](fab: ListTuple2[A, B],
                             lc: Eval[C]
    )(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] = {
      def loop(abs: ListTuple2[A, B]): Eval[C] =
        abs.value match {
          case Nil         => lc
          case (a, b) :: t => f(a, g(b, Eval.defer(loop(ListTuple2(t)))))
        }
      Eval.defer(loop(fab))
    }
    def bitraverse[G[_], A, B, C, D](
      fab: ListTuple2[A, B]
    )(f: A => G[C], g: B => G[D])(implicit G: Applicative[G]): G[ListTuple2[C, D]] = {
      def loop(abs: ListTuple2[A, B]): Eval[G[ListTuple2[C, D]]] =
        abs.value match {
          case Nil => Now(G.pure(ListTuple2(List.empty)))
          case (a, b) :: t =>
            G.map2Eval(G.product(f(a), g(b)), Eval.defer(loop(ListTuple2(t))))((cur, acc) =>
              ListTuple2(cur :: acc.value)
            )
        }
      loop(fab).value
    }
  }

  test("ParBisequence Either should accumulate errors") {
    forAll { (es: List[(Either[String, Int], Either[String, Int])]) =>
      val lefts = es
        .flatMap { case (a, b) =>
          List(a, b)
        }
        .collect { case Left(e) =>
          e
        }
        .foldMap(identity)

      assert(ListTuple2(es).parBisequence.fold(identity, i => Monoid[String].empty) === lefts)
    }
  }

  test("ParBisequence Ior should accumulate errors") {
    forAll { (es: List[(Ior[String, Int], Ior[String, Int])]) =>
      val lefts = es
        .flatMap { case (a, b) =>
          List(a, b)
        }
        .map(_.left)
        .collect { case Some(e) =>
          e
        }
        .foldMap(identity)

      assert(ListTuple2(es).parBisequence.left.getOrElse(Monoid[String].empty) === lefts)
    }
  }

  test("ParBisequence Ior should bisequence values") {
    forAll { (es: List[(Ior[String, Int], Ior[String, Int])]) =>
      val wrapped = ListTuple2(es)
      assert(wrapped.parBisequence.right.map(_.value) === wrapped.bimap(_.toOption, _.toOption).bisequence.map(_.value))
    }
  }

  test("ParBitraverse identity should be equivalent to parBisequence") {
    forAll { (es: (Either[String, Int], Either[String, Long])) =>
      assert(es.parBitraverse(identity, identity) === (es.parBisequence))
    }
  }

  test("ParLeftSequence Either should accumulate errors") {
    forAll { (es: List[(Either[String, Int], Int)]) =>
      val lefts = es
        .collect { case (Left(e), _) =>
          e
        }
        .foldMap(identity)

      assert(ListTuple2(es).parLeftSequence.fold(identity, i => Monoid[String].empty) === lefts)
    }
  }

  test("ParLeftSequence Ior should accumulate errors") {
    forAll { (es: List[(Ior[String, Int], Int)]) =>
      val lefts = es
        .map { case (a, b) =>
          a.left
        }
        .collect { case Some(e) =>
          e
        }
        .foldMap(identity)

      assert(ListTuple2(es).parLeftSequence.left.getOrElse(Monoid[String].empty) === lefts)
    }
  }

  test("ParLeftSequence Ior should leftSequence values") {
    forAll { (es: List[(Ior[String, Int], Int)]) =>
      val wrapped = ListTuple2(es)
      assert(
        wrapped.parLeftSequence.right.map(_.value) === (wrapped.bimap(_.toOption, identity).leftSequence.map(_.value))
      )
    }
  }

  test("ParLeftTraverse identity should be equivalent to parLeftSequence") {
    forAll { (es: (Either[String, Int], Either[String, Long])) =>
      assert(es.parLeftTraverse(identity) === (es.parLeftSequence))
    }
  }

  test("ParFlatTraverse should be equivalent to parTraverse map flatten") {
    forAll { (es: List[Either[String, Int]]) =>
      val f: Int => List[Int] = i => List(i, i + 1)
      assert(
        Parallel.parFlatTraverse(es)(e => e.map(f))
          === es.parTraverse(e => e.map(f)).map(_.flatten)
      )
    }
  }

  test("ParFlatTraverse identity should be equivalent to parFlatSequence") {
    forAll { (es: List[Either[String, List[Int]]]) =>
      assert(Parallel.parFlatTraverse(es)(identity) === (Parallel.parFlatSequence(es)))
    }
  }

  test("ParFlatSequence syntax should be equivalent to Parallel.parFlatSequence") {
    forAll { (es: List[Either[String, List[Int]]]) =>
      assert(es.parFlatSequence === (Parallel.parFlatSequence(es)))
    }
  }

  test("ParFlatTraverse syntax should be equivalent to Parallel.parFlatTraverse") {
    forAll { (es: List[Either[String, Int]]) =>
      val f: Int => List[Int] = i => List(i, i + 1)
      assert(
        Parallel.parFlatTraverse(es)(e => e.map(f))
          === (es.parFlatTraverse(e => e.map(f)))
      )
    }
  }

  test("ParNonEmptyFlatTraverse should be equivalent to parNonEmptyTraverse map flatten") {
    forAll { (es: NonEmptyList[Either[String, Int]]) =>
      val f: Int => NonEmptyList[Int] = i => NonEmptyList.of(i, i + 1)
      assert(
        Parallel.parNonEmptyFlatTraverse(es)(e => e.map(f))
          === (Parallel.parNonEmptyTraverse(es)(e => e.map(f)).map(_.flatten))
      )
    }
  }

  test("ParNonEmptyFlatTraverse identity should be equivalent to parNonEmptyFlatSequence") {
    forAll { (es: NonEmptyList[Either[String, NonEmptyList[Int]]]) =>
      assert(Parallel.parNonEmptyFlatTraverse(es)(identity) === (Parallel.parNonEmptyFlatSequence(es)))
    }
  }

  test("ParFoldMapA should be equivalent to parTraverse map combineAll (where it exists)") {
    forAll { (es: List[Int], f: Int => Either[String, String]) =>
      assert(
        Parallel.parFoldMapA(es)(f) ===
          Parallel.parTraverse(es)(f).map(_.combineAll)
      )
    }
  }

  test("ParReduceMapA should be equivalent to parNonEmptyTraverse map reduce (where it exists)") {
    forAll { (es: NonEmptyList[Int], f: Int => Either[String, String]) =>
      assert(
        Parallel.parReduceMapA(es)(f) ===
          Parallel.parNonEmptyTraverse(es)(f).map(_.reduce)
      )
    }
  }

  test("parAp accumulates errors in order") {
    val right: Either[String, Int => Int] = Left("Hello")
    assert(Parallel.parAp(right)("World".asLeft) === (Left("HelloWorld")))
  }

  test("parAp2 accumulates errors in order") {
    val plus = (_: Int) + (_: Int)
    val rightPlus: Either[String, (Int, Int) => Int] = Right(plus)
    assert(Parallel.parAp2(rightPlus)("Hello".asLeft, "World".asLeft) === (Left("HelloWorld")))
  }

  test("ParReplicateA should be equivalent to fill parSequence") {
    forAll(Gen.choose(1, 20), Arbitrary.arbitrary[Either[String, String]]) {
      (repetitions: Int, e: Either[String, String]) =>
        assert(Parallel.parReplicateA(repetitions, e) === Parallel.parSequence(List.fill(repetitions)(e)))
    }
  }

  test("ParReplicateA_ should be equivalent to fill parSequenceVoid") {
    forAll(Gen.choose(1, 20), Arbitrary.arbitrary[Either[String, String]]) {
      (repetitions: Int, e: Either[String, String]) =>
        assert(Parallel.parReplicateA_(repetitions, e) === Parallel.parSequenceVoid(List.fill(repetitions)(e)))
    }
  }

  test("ParReplicateA 2 should be equivalent to parMap2 List") {
    forAll { (e: Either[String, String]) =>
      assert(Parallel.parReplicateA(2, e) === Parallel.parMap2(e, e)((s1, s2) => List(s1, s2)))
    }
  }

  test("ParReplicateA_ 2 should be equivalent to parMap2.void List") {
    forAll { (e: Either[String, String]) =>
      assert(Parallel.parReplicateA_(2, e) === Parallel.parMap2(e, e)((s1, s2) => List(s1, s2)).void)
    }
  }

  test("Kleisli with Either should accumulate errors") {
    val k1: Kleisli[Either[String, *], String, Int] = Kleisli(s => Right(s.length))
    val k2: Kleisli[Either[String, *], String, Int] = Kleisli(s => Left("Boo"))
    val k3: Kleisli[Either[String, *], String, Int] = Kleisli(s => Left("Nope"))

    assert((List(k1, k2, k3).parSequence.run("Hello")) === (Left("BooNope")))

  }

  test("WriterT with Either should accumulate errors") {
    val w1: WriterT[Either[String, *], String, Int] = WriterT.liftF(Left("Too "))
    val w2: WriterT[Either[String, *], String, Int] = WriterT.liftF(Left("bad."))

    assert(((w1, w2).parMapN(_ + _).value) === (Left("Too bad.")))

  }

  test("ParMap over NonEmptyList should be consistent with zip") {
    forAll { (as: NonEmptyList[Int], bs: NonEmptyList[Int], cs: NonEmptyList[Int]) =>
      assert((as, bs, cs).parMapN(_ + _ + _) === (as.zipWith(bs)(_ + _).zipWith(cs)(_ + _)))
    }
  }

  test("ParMap over NonEmptyVector should be consistent with zip") {
    forAll { (as: NonEmptyVector[Int], bs: NonEmptyVector[Int], cs: NonEmptyVector[Int]) =>
      assert((as, bs, cs).parMapN(_ + _ + _) === (as.zipWith(bs)(_ + _).zipWith(cs)(_ + _)))
    }
  }

  test("ParMap over List should be consistent with zip") {
    forAll { (as: List[Int], bs: List[Int], cs: List[Int]) =>
      val zipped = as
        .zip(bs)
        .map { case (a, b) =>
          a + b
        }
        .zip(cs)
        .map { case (a, b) =>
          a + b
        }

      assert((as, bs, cs).parMapN(_ + _ + _) === zipped)
    }
  }

  test("ParMap over Vector should be consistent with zip") {
    forAll { (as: Vector[Int], bs: Vector[Int], cs: Vector[Int]) =>
      val zipped = as
        .zip(bs)
        .map { case (a, b) =>
          a + b
        }
        .zip(cs)
        .map { case (a, b) =>
          a + b
        }

      assert((as, bs, cs).parMapN(_ + _ + _) === zipped)
    }
  }

  test("ParFlatMapN over List should be consistent with parMapN flatten") {
    forAll { (as: List[Int], bs: List[Int], cs: List[Int], mf: (Int, Int, Int) => List[Int]) =>
      assert((as, bs, cs).parFlatMapN(mf) == (as, bs, cs).parMapN(mf).flatten)
    }
  }

  test("ParFlatMapN over NonEmptyList should be consistent with parMapN flatten") {
    forAll {
      (as: NonEmptyList[Int], bs: NonEmptyList[Int], cs: NonEmptyList[Int], mf: (Int, Int, Int) => NonEmptyList[Int]) =>
        assert((as, bs, cs).parFlatMapN(mf) == (as, bs, cs).parMapN(mf).flatten)
    }
  }

  test("ParFlatMap over List should be consistent with flatmap") {
    forAll { (as: List[Int], mf: Int => List[Int]) =>
      assert(Tuple1(as).parFlatMap(mf) == Tuple1(as).flatMap(mf))
    }
  }

  test("ParFlatMap over NonEmptyList should be consistent with flatmap") {
    forAll { (as: NonEmptyList[Int], mf: Int => NonEmptyList[Int]) =>
      assert(Tuple1(as).parFlatMap(mf) == Tuple1(as).flatMap(mf))
    }
  }

  test("ParMapN over f should be consistent with parFlatMapN over f lifted in List") {
    forAll { (as: List[Int], bs: List[Int], f: (Int, Int) => Int) =>
      val mf: (Int, Int) => List[Int] = (a, b) => f(a, b).pure[List]
      assert((as, bs).parMapN(f) == (as, bs).parFlatMapN(mf))
    }
  }

  test("ParTupled of NonEmptyList should be consistent with ParMap of Tuple.apply") {
    forAll { (fa: NonEmptyList[Int], fb: NonEmptyList[Int], fc: NonEmptyList[Int], fd: NonEmptyList[Int]) =>
      assert((fa, fb, fc, fd).parTupled === ((fa, fb, fc, fd).parMapN(Tuple4.apply)))
    }
  }

  test("ParTupled of NonEmptyVector should be consistent with ParMap of Tuple.apply") {
    forAll { (fa: NonEmptyVector[Int], fb: NonEmptyVector[Int], fc: NonEmptyVector[Int], fd: NonEmptyVector[Int]) =>
      assert((fa, fb, fc, fd).parTupled === ((fa, fb, fc, fd).parMapN(Tuple4.apply)))
    }
  }

  test("ParTupled of List should be consistent with ParMap of Tuple.apply") {
    forAll { (fa: List[Int], fb: List[Int], fc: List[Int], fd: List[Int]) =>
      assert((fa, fb, fc, fd).parTupled === ((fa, fb, fc, fd).parMapN(Tuple4.apply)))
    }
  }

  test("ParTupled of Vector should be consistent with ParMap of Tuple.apply") {
    forAll { (fa: Vector[Int], fb: Vector[Int], fc: Vector[Int], fd: Vector[Int]) =>
      assert((fa, fb, fc, fd).parTupled === ((fa, fb, fc, fd).parMapN(Tuple4.apply)))
    }
  }

  test("ParTupled of List should be consistent with zip") {
    forAll { (fa: List[Int], fb: List[Int], fc: List[Int], fd: List[Int]) =>
      assert((fa, fb, fc, fd).parTupled === fa.zip(fb).zip(fc).zip(fd).map { case (((a, b), c), d) => (a, b, c, d) })
    }
  }

  test("ParTupled of Vector should be consistent with zip") {
    forAll { (fa: Vector[Int], fb: Vector[Int], fc: Vector[Int], fd: Vector[Int]) =>
      assert((fa, fb, fc, fd).parTupled === fa.zip(fb).zip(fc).zip(fd).map { case (((a, b), c), d) => (a, b, c, d) })
    }
  }

  test("IorT leverages parallel effect instances when it exists") {
    case class Marker(value: String) extends java.lang.Exception("marker") {
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

    assert(resultSansInstance === ("sequential".some))
    assert(resultWithInstance === ("parallel".some))
  }
  test("Parallel[IorT[F, E, *]] applies Ior's additive effect when F has no Parallel") {
    forAll { (intI: Int) =>
      val iorT = IorT.leftT[Option, Boolean](intI)
      val parComposed = (iorT, iorT).parMapN(_ && _)
      parComposed === IorT.leftT[Option, Boolean](intI + intI)
    }
  }
  test("Parallel[IorT[F, E, *]] does not apply Ior's additive effect when F has Parallel") {
    forAll { (intI: Int) =>
      val iorT = IorT.leftT[Either[Int, *], Boolean](intI)
      val parComposed = (iorT, iorT).parMapN(_ && _)
      parComposed === iorT
    }
  }
  checkAll("Parallel[Either[String, *]", ParallelTests[Either[String, *]].parallel[Int, String])
  checkAll("Parallel[Ior[String, *]]", ParallelTests[Ior[String, *]].parallel[Int, String])
  checkAll(
    "Parallel[IorT[F, String, *]] with parallel effect",
    ParallelTests[IorT[Either[String, *], String, *]].parallel[Int, String]
  )
  checkAll(
    "Parallel[IorT[F, String, *]] with parallel effect (accumulating)", {
      type IE[A] = IorT[Either[String, *], String, A]
      type IV[A] = IorT[Validated[String, *], String, A]
      implicit val iorTParallel: Parallel.Aux[IE, IV] = IorT.accumulatingParallel[Either[String, *], String]
      ParallelTests[IorT[Either[String, *], String, *]].parallel[Int, String]
    }
  )
  checkAll(
    "Parallel[IorT[F, String, *]] with sequential effect",
    ParallelTests[IorT[Option, String, *]].parallel[Int, String]
  )
  checkAll("Parallel[OptionT[M, *]]", ParallelTests[OptionT[Either[String, *], *]].parallel[Int, String])

  test("Parallel[EitherT[F, E, *]] applies Validated's additive effect when F has no Parallel") {
    forAll { (intI: Int) =>
      val eitherT = EitherT.leftT[Option, Boolean](intI)
      val parComposed = (eitherT, eitherT).parMapN(_ && _)
      parComposed === EitherT.leftT[Option, Boolean](intI + intI)
    }
  }
  test("Parallel[EitherT[F, E, *]] does not apply Validated's additive effect when F has Parallel") {
    forAll { (intI: Int) =>
      val eitherT = EitherT.leftT[Ior[Int, *], Boolean](intI)
      val parComposed = (eitherT, eitherT).parMapN(_ && _)
      parComposed === eitherT
    }
  }
  checkAll(
    "Parallel[EitherT[M, String, *]]",
    ParallelTests[EitherT[Either[String, *], String, *]]
      .parallel[Int, String]
  )
  checkAll(
    "Parallel[EitherT[M, String, *]] (accumulating)", {
      type EE[A] = EitherT[Either[String, *], String, A]
      type VV[A] = Nested[Validated[String, *], Validated[String, *], A]
      implicit val eitherTParallel: Parallel.Aux[EE, VV] = EitherT.accumulatingParallel[Either[String, *], String]
      ParallelTests[EitherT[Either[String, *], String, *]].parallel[Int, String]
    }
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

  checkAll("NonEmptyParallel[NonEmptyVector]", NonEmptyParallelTests[NonEmptyVector].nonEmptyParallel[Int, String])

  checkAll("NonEmptyParallel[NonEmptyList]", NonEmptyParallelTests[NonEmptyList].nonEmptyParallel[Int, String])

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
}

@annotation.nowarn("cat=deprecation")
sealed trait ParallelSuiteStreamSpecific { self: ParallelSuite =>

  test("ParMap over Stream should be consistent with zip") {
    forAll { (as: Stream[Int], bs: Stream[Int], cs: Stream[Int]) =>
      val zipped = as
        .zip(bs)
        .map { case (a, b) =>
          a + b
        }
        .zip(cs)
        .map { case (a, b) =>
          a + b
        }

      assert((as, bs, cs).parMapN(_ + _ + _) === zipped)
    }
  }

  test("ParTupled of Stream should be consistent with ParMap of Tuple.apply") {
    forAll { (fa: Stream[Int], fb: Stream[Int], fc: Stream[Int], fd: Stream[Int]) =>
      assert((fa, fb, fc, fd).parTupled === ((fa, fb, fc, fd).parMapN(Tuple4.apply)))
    }
  }

  test("ParTupled of Stream should be consistent with zip") {
    forAll { (fa: Stream[Int], fb: Stream[Int], fc: Stream[Int], fd: Stream[Int]) =>
      assert((fa, fb, fc, fd).parTupled === fa.zip(fb).zip(fc).zip(fd).map { case (((a, b), c), d) => (a, b, c, d) })
    }
  }

  // Can't test Parallel here, as Applicative[ZipStream].pure doesn't terminate
  checkAll("Parallel[Stream]", NonEmptyParallelTests[Stream].nonEmptyParallel[Int, String])

  // TODO this doesn't infer?
  checkAll("Parallel[NonEmptyStream]", ParallelTests[NonEmptyStream, OneAnd[ZipStream, *]].parallel[Int, String])

  test("Parallel.apply should return an appropriately typed instance given both type parameters") {
    val p1: Parallel.Aux[Either[String, *], Validated[String, *]] = Parallel[Either[String, *], Validated[String, *]]
    val p2: Parallel.Aux[Stream, ZipStream] = Parallel[Stream, ZipStream]
  }

  test("Parallel.apply should return an appropriately typed instance given the first type parameter") {
    val p1: Parallel.Aux[Either[String, *], Validated[String, *]] = Parallel[Either[String, *], Validated[String, *]]
    val p2: Parallel.Aux[Stream, ZipStream] = Parallel[Stream]
  }
}

trait ApplicativeErrorForEitherTest extends munit.DisciplineSuite {
  implicit def eqV[A: Eq, B: Eq]: Eq[Validated[A, B]] = cats.data.Validated.catsDataEqForValidated

  {
    implicit val parVal: ApplicativeError[Validated[String, *], String] =
      Parallel.applicativeError[Either[String, *], String]

    checkAll("ApplicativeError[Validated[String, Int]]",
             ApplicativeErrorTests[Validated[String, *], String].applicativeError[Int, Int, Int]
    )
  }
}
