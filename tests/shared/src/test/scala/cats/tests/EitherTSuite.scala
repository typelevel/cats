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

import cats._
import cats.data.{EitherT, Ior, IorT, State}
import cats.kernel.laws.discipline.{EqTests, MonoidTests, OrderTests, PartialOrderTests, SemigroupTests}
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.either._
import scala.util.{Failure, Success, Try}
import cats.syntax.eq._
import org.scalacheck.Prop._

class EitherTSuite extends CatsSuite {
  implicit val iso: Isomorphisms[EitherT[ListWrapper, String, *]] = Isomorphisms
    .invariant[EitherT[ListWrapper, String, *]](EitherT.catsDataFunctorForEitherT(ListWrapper.functor))

  // Test instance summoning
  def summon[F[_]: Traverse](): Unit = {
    Bifunctor[EitherT[F, *, *]]
    Bifoldable[EitherT[F, *, *]]
  }

  checkAll("EitherT[Eval, String, *]", DeferTests[EitherT[Eval, String, *]].defer[Int])

  {
    checkAll("EitherT[Option, ListWrapper[String], *]",
             SemigroupKTests[EitherT[Option, ListWrapper[String], *]].semigroupK[Int]
    )
    checkAll("SemigroupK[EitherT[Option, ListWrapper[String], *]]",
             SerializableTests.serializable(SemigroupK[EitherT[Option, ListWrapper[String], *]])
    )
  }

  {
    implicit val F: Order[ListWrapper[Either[String, Int]]] = ListWrapper.order[Either[String, Int]]

    checkAll("EitherT[List, String, Int]", OrderTests[EitherT[ListWrapper, String, Int]].order)
    checkAll("Order[EitherT[List, String, Int]]",
             SerializableTests.serializable(Order[EitherT[ListWrapper, String, Int]])
    )
  }

  {
    // if a Functor for F is defined
    implicit val F: Functor[ListWrapper] = ListWrapper.functor

    checkAll("EitherT[ListWrapper, *, *]",
             BifunctorTests[EitherT[ListWrapper, *, *]].bifunctor[Int, Int, Int, String, String, String]
    )
    checkAll("Bifunctor[EitherT[ListWrapper, *, *]]",
             SerializableTests.serializable(Bifunctor[EitherT[ListWrapper, *, *]])
    )
    checkAll("EitherT[ListWrapper, Int, *]", FunctorTests[EitherT[ListWrapper, Int, *]].functor[Int, Int, Int])
    checkAll("Functor[EitherT[ListWrapper, Int, *]]",
             SerializableTests.serializable(Functor[EitherT[ListWrapper, Int, *]])
    )
  }

  {
    // if a Foldable for F is defined
    implicit val F: Foldable[ListWrapper] = ListWrapper.foldable

    checkAll("EitherT[ListWrapper, Int, *]", FoldableTests[EitherT[ListWrapper, Int, *]].foldable[Int, Int])
    checkAll("Foldable[EitherT[ListWrapper, Int, *]]",
             SerializableTests.serializable(Foldable[EitherT[ListWrapper, Int, *]])
    )
    checkAll("EitherT[ListWrapper, *, *]", BifoldableTests[EitherT[ListWrapper, *, *]].bifoldable[Int, Int, Int])
    checkAll("Bifoldable[EitherT[ListWrapper, *, *]]",
             SerializableTests.serializable(Bifoldable[EitherT[ListWrapper, *, *]])
    )
  }

  {
    // if a Traverse for F is defined
    implicit val F: Traverse[ListWrapper] = ListWrapper.traverse

    checkAll("EitherT[ListWrapper, Int, *]",
             TraverseTests[EitherT[ListWrapper, Int, *]].traverse[Int, Int, Int, Int, Option, Option]
    )
    checkAll("Traverse[EitherT[ListWrapper, Int, *]]",
             SerializableTests.serializable(Traverse[EitherT[ListWrapper, Int, *]])
    )
    checkAll("EitherT[ListWrapper, *, *]",
             BitraverseTests[EitherT[ListWrapper, *, *]].bitraverse[Option, Int, Int, Int, String, String, String]
    )
    checkAll("Bitraverse[EitherT[ListWrapper, *, *]]",
             SerializableTests.serializable(Bitraverse[EitherT[ListWrapper, *, *]])
    )
  }

  {
    // if a Monad is defined

    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    implicit val eq0: Eq[EitherT[ListWrapper, String, Either[String, Int]]] =
      EitherT.catsDataEqForEitherT[ListWrapper, String, Either[String, Int]]
    implicit val eq1: Eq[EitherT[EitherT[ListWrapper, String, *], String, Int]] =
      EitherT.catsDataEqForEitherT[EitherT[ListWrapper, String, *], String, Int](eq0)

    Functor[EitherT[ListWrapper, String, *]]
    Applicative[EitherT[ListWrapper, String, *]]
    Monad[EitherT[ListWrapper, String, *]]

    checkAll("EitherT[ListWrapper, String, Int]",
             MonadErrorTests[EitherT[ListWrapper, String, *], String].monadError[Int, Int, Int]
    )
    checkAll("MonadError[EitherT[List, *, *]]",
             SerializableTests.serializable(MonadError[EitherT[ListWrapper, String, *], String])
    )

  }

  {
    // if a MonadError is defined
    // Tests for catsDataMonadErrorFForEitherT instance, for recovery on errors of F.

    implicit val eq1: Eq[EitherT[Option, String, Either[Unit, String]]] =
      EitherT.catsDataEqForEitherT[Option, String, Either[Unit, String]]
    implicit val eq2: Eq[EitherT[EitherT[Option, String, *], Unit, String]] =
      EitherT.catsDataEqForEitherT[EitherT[Option, String, *], Unit, String](eq1)
    implicit val me: MonadError[EitherT[Option, String, *], Unit] =
      EitherT.catsDataMonadErrorFForEitherT[Option, Unit, String](cats.instances.option.catsStdInstancesForOption)

    Functor[EitherT[Option, String, *]]
    Applicative[EitherT[Option, String, *]]
    Monad[EitherT[Option, String, *]]

    checkAll("EitherT[Option, String, String]",
             MonadErrorTests[EitherT[Option, String, *], Unit].monadError[String, String, String]
    )
    checkAll("MonadError[EitherT[Option, *, *]]",
             SerializableTests.serializable(MonadError[EitherT[Option, String, *], Unit])
    )
  }

  {
    // if a Monad is defined
    implicit val F: Monad[ListWrapper] = ListWrapper.monad

    Functor[EitherT[ListWrapper, String, *]]
    Applicative[EitherT[ListWrapper, String, *]]
    Monad[EitherT[ListWrapper, String, *]]

    checkAll("EitherT[ListWrapper, String, Int]", MonadTests[EitherT[ListWrapper, String, *]].monad[Int, Int, Int])
    checkAll("Monad[EitherT[ListWrapper, String, *]]",
             SerializableTests.serializable(Monad[EitherT[ListWrapper, String, *]])
    )
  }

  {
    // if a foldable is defined
    implicit val F: Foldable[ListWrapper] = ListWrapper.foldable

    checkAll("EitherT[ListWrapper, Int, *]", FoldableTests[EitherT[ListWrapper, Int, *]].foldable[Int, Int])
    checkAll("Foldable[EitherT[ListWrapper, Int, *]]",
             SerializableTests.serializable(Foldable[EitherT[ListWrapper, Int, *]])
    )
  }

  {
    implicit val F: PartialOrder[ListWrapper[Either[String, Int]]] = ListWrapper.partialOrder[Either[String, Int]]

    checkAll("EitherT[ListWrapper, String, Int]", PartialOrderTests[EitherT[ListWrapper, String, Int]].partialOrder)
    checkAll("PartialOrder[EitherT[ListWrapper, String, Int]]",
             SerializableTests.serializable(PartialOrder[EitherT[ListWrapper, String, Int]])
    )
  }

  {
    implicit val F: Semigroup[ListWrapper[Either[String, Int]]] = ListWrapper.semigroup[Either[String, Int]]

    checkAll("EitherT[ListWrapper, String, Int]", SemigroupTests[EitherT[ListWrapper, String, Int]].semigroup)
    checkAll("Semigroup[EitherT[ListWrapper, String, Int]]",
             SerializableTests.serializable(Semigroup[EitherT[ListWrapper, String, Int]])
    )
  }

  {
    implicit val F: Monoid[ListWrapper[Either[String, Int]]] = ListWrapper.monoid[Either[String, Int]]

    Semigroup[EitherT[ListWrapper, String, Int]]

    checkAll("EitherT[ListWrapper, String, Int]", MonoidTests[EitherT[ListWrapper, String, Int]].monoid)
    checkAll("Monoid[EitherT[ListWrapper, String, Int]]",
             SerializableTests.serializable(Monoid[EitherT[ListWrapper, String, Int]])
    )
  }

  {
    implicit val F: Eq[ListWrapper[Either[String, Int]]] = ListWrapper.eqv[Either[String, Int]]

    checkAll("EitherT[ListWrapper, String, Int]", EqTests[EitherT[ListWrapper, String, Int]].eqv)
    checkAll("Eq[EitherT[ListWrapper, String, Int]]",
             SerializableTests.serializable(Eq[EitherT[ListWrapper, String, Int]])
    )
  }

  test("toValidated") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      assert(eithert.toValidated.map(_.toEither) === (eithert.value))
    }
  }

  test("toValidatedNel") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      assert(eithert.toValidatedNel.map(_.toEither.leftMap(_.head)) === (eithert.value))
    }
  }

  test("toValidatedNec") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      assert(eithert.toValidatedNec.map(_.toEither.leftMap(_.head)) === (eithert.value))
    }
  }

  test("toNested") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      assert(eithert.toNested.value === (eithert.value))
    }
  }

  test("toNestedValidated") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      assert(eithert.toNestedValidated.value === (eithert.value.map(_.toValidated)))
    }
  }

  test("toNestedValidatedNel") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      assert(eithert.toNestedValidatedNel.value === (eithert.value.map(_.toValidatedNel)))
    }
  }

  test("toNestedValidatedNec") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      assert(eithert.toNestedValidatedNec.value === (eithert.value.map(_.toValidatedNec)))
    }
  }

  test("withValidated") {
    forAll { (eithert: EitherT[List, String, Int], f: String => Char, g: Int => Double) =>
      assert(eithert.withValidated(_.bimap(f, g)) === (eithert.bimap(f, g)))
    }
  }

  test("fromEither") {
    forAll { (either: Either[String, Int]) =>
      assert((Some(either.isLeft): Option[Boolean]) === (EitherT.fromEither[Option](either).isLeft))
    }
  }

  test("fromOption isLeft consistent with Option.isEmpty") {
    forAll { (o: Option[Int], s: String) =>
      assert(EitherT.fromOption[Id](o, s).isLeft === (o.isEmpty))
    }
  }

  test("fromOptionF isLeft consistent with Option isEmpty") {
    forAll { (option: Option[Int], s: String) =>
      assert(EitherT.fromOptionF[Id, String, Int](option, s).isLeft === (option.isEmpty))
    }
  }

  test("fromOptionM consistent with fromOptionF") {
    forAll { (option: Option[Int], s: String) =>
      assert(EitherT.fromOptionM[Id, String, Int](option, s) === (EitherT.fromOptionF[Id, String, Int](option, s)))
    }
  }

  test("cond consistent with Either.cond") {
    forAll { (cond: Boolean, s: String, i: Int) =>
      assert(EitherT.cond[Id](cond, s, i).value === (Either.cond(cond, s, i)))
    }
  }

  test("isLeft negation of isRight") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      assert(eithert.isLeft === (eithert.isRight.map(!_)))
    }
  }

  test("double swap is noop") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      assert(eithert.swap.swap === eithert)
    }
  }

  test("swap negates isRight") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      assert(eithert.swap.isRight === (eithert.isRight.map(!_)))
    }
  }

  test("toOption on Right returns Some") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      assert(eithert.toOption.isDefined === (eithert.isRight))
    }
  }

  test("toEither preserves isRight") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      assert(eithert.value.map(_.isRight) === (eithert.isRight))
    }
  }

  test("recover recovers handled values") {
    val eithert = EitherT.leftT[Id, Int]("eithert")
    assert(eithert.recover { case "eithert" => 5 }.isRight === true)
  }

  test("recover ignores unhandled values") {
    val eithert = EitherT.leftT[Id, Int]("eithert")
    assert(eithert.recover { case "noteithert" => 5 } === eithert)
  }

  test("recover ignores the right side") {
    val eithert = EitherT.pure[Id, String](10)
    assert(eithert.recover { case "eithert" => 5 } === eithert)
  }

  test("recoverWith recovers handled values") {
    val eithert = EitherT.leftT[Id, Int]("eithert")
    assert(eithert.recoverWith { case "eithert" => EitherT.pure[Id, String](5) }.isRight === true)
  }

  test("recoverWith ignores unhandled values") {
    val eithert = EitherT.leftT[Id, Int]("eithert")
    assert(eithert.recoverWith { case "noteithert" => EitherT.pure[Id, String](5) } === eithert)
  }

  test("rethrowT is inverse of attemptT when applied to a successful value") {
    implicit val eqThrow: Eq[Throwable] = Eq.fromUniversalEquals
    val success: Try[Int] = Success(42)

    assert(success.attemptT.rethrowT === success)
  }

  test("rethrowT is inverse of attemptT when applied to a failed value") {
    implicit val eqThrow: Eq[Throwable] = Eq.fromUniversalEquals
    val failed: Try[Int] = Failure(new IllegalArgumentException("error"))

    assert(failed.attemptT.rethrowT === failed)
  }

  test("rethrowT works with specialized failures") {
    implicit val eqThrow: Eq[Throwable] = Eq.fromUniversalEquals
    val failed: Try[Int] = Failure(new IllegalArgumentException("error"))

    val t: EitherT[Try, IllegalArgumentException, Int] =
      failed.attemptT.leftMap(_.asInstanceOf[IllegalArgumentException])
    assert(t.rethrowT === failed)
  }

  test("transform consistent with value.map") {
    forAll { (eithert: EitherT[List, String, Int], f: Either[String, Int] => Either[Long, Double]) =>
      assert(eithert.transform(f) === (EitherT(eithert.value.map(f))))
    }
  }

  test("mapK consistent with f(value)+pure") {
    val f: List ~> Option = new (List ~> Option) { def apply[A](a: List[A]): Option[A] = a.headOption }
    forAll { (eithert: EitherT[List, String, Int]) =>
      assert(eithert.mapK(f) === (EitherT(f(eithert.value))))
    }
  }

  test("semiflatMap consistent with value.flatMap+f+pure") {
    forAll { (eithert: EitherT[List, String, Int], f: Int => List[String]) =>
      assert(eithert.semiflatMap(f) === (EitherT(eithert.value.flatMap {
        case l @ Left(_) => List(l.asInstanceOf[Either[String, String]])
        case Right(b)    => f(b).map(Right(_))
      })))
    }
  }

  test("subflatMap consistent with value.map+flatMap") {
    forAll { (eithert: EitherT[List, String, Int], f: Int => Either[String, Double]) =>
      assert(eithert.subflatMap(f) === (EitherT(eithert.value.map(_.flatMap(f)))))
    }
  }

  test("flatMap and flatMapF consistent") {
    forAll { (eithert: EitherT[List, String, Int], f: Int => EitherT[List, String, Int]) =>
      assert(eithert.flatMap(f) === (eithert.flatMapF(f(_).value)))
    }
  }

  test("fold with Id consistent with Either fold") {
    forAll { (eithert: EitherT[Id, String, Int], f: String => Long, g: Int => Long) =>
      assert(eithert.fold(f, g) === (eithert.value.fold(f, g)))
    }
  }

  test("foldF with Id consistent with Either fold") {
    forAll { (eithert: EitherT[Id, String, Int], f: String => Long, g: Int => Long) =>
      assert(eithert.foldF(f, g) === (eithert.value.fold(f, g)))
    }
  }

  test("valueOr with Id consistent with Either valueOr") {
    forAll { (eithert: EitherT[Id, String, Int], f: String => Int) =>
      assert(eithert.valueOr(f) === (eithert.value.valueOr(f)))
    }
  }

  test("valueOrF with Id consistent with Either valueOr") {
    forAll { (eithert: EitherT[Id, String, Int], f: String => Int) =>
      assert(eithert.valueOrF(f) === (eithert.value.valueOr(f)))
    }
  }

  test("getOrElse with Id consistent with Either getOrElse") {
    forAll { (eithert: EitherT[Id, String, Int], i: Int) =>
      assert(eithert.getOrElse(i) === (eithert.value.getOrElse(i)))
    }
  }

  test("getOrElseF with Id consistent with Either getOrElse") {
    forAll { (eithert: EitherT[Id, String, Int], i: Int) =>
      assert(eithert.getOrElseF(i) === (eithert.value.getOrElse(i)))
    }
  }

  test("getOrRaise consistent with EitherT.getOrElseF(F.raiseError(e))") {
    forAll { (eithert: EitherT[Either[String, *], String, Int], error: String) =>
      assertEquals(
        obtained = eithert.getOrRaise(error),
        expected = eithert.getOrElseF(Left(error))
      )
    }
  }

  test("getOrRaise consistent with EitherT.leftMap(_ => error).rethrowT") {
    forAll { (eithert: EitherT[Either[String, *], String, Int], error: String) =>
      assertEquals(
        obtained = eithert.getOrRaise(error),
        expected = eithert.leftMap(_ => error).rethrowT
      )
    }
  }

  test("orElse with Id consistent with Either orElse") {
    forAll { (eithert: EitherT[Id, String, Int], fallback: EitherT[Id, String, Int]) =>
      assert(eithert.orElse(fallback).value === (eithert.value.orElse(fallback.value)))
    }
  }

  test("orElse evaluates effect only once") {
    forAll { (either: Either[String, Int], fallback: EitherT[Eval, String, Int]) =>
      var evals = 0
      val eithert = EitherT(Eval.always { evals += 1; either }).orElse(fallback)
      eithert.value.value
      assert(evals === 1)
    }
  }

  test("forall with Id consistent with Either forall") {
    forAll { (eithert: EitherT[Id, String, Int], f: Int => Boolean) =>
      assert(eithert.forall(f) === (eithert.value.forall(f)))
    }
  }

  test("exists with Id consistent with Either exists") {
    forAll { (eithert: EitherT[Id, String, Int], f: Int => Boolean) =>
      assert(eithert.exists(f) === (eithert.value.exists(f)))
    }
  }

  test("leftMap with Id consistent with Either leftMap") {
    forAll { (eithert: EitherT[Id, String, Int], f: String => Long) =>
      assert(eithert.leftMap(f).value === (eithert.value.leftMap(f)))
    }
  }

  test("compare with Id consistent with Either compare") {
    forAll { (x: EitherT[Id, String, Int], y: EitherT[Id, String, Int]) =>
      assert(x.compare(y) === (x.value.compare(y.value)))
    }
  }

  test("=== with Id consistent with Either ===") {
    forAll { (x: EitherT[Id, String, Int], y: EitherT[Id, String, Int]) =>
      assert(x === y === (x.value === y.value))
    }
  }

  test("traverse with Id consistent with Either traverse") {
    forAll { (x: EitherT[Id, String, Int], f: Int => Option[Long]) =>
      val e: Either[String, Int] = x.value
      assert(x.traverse(f).map(_.value) === (e.traverse(f)))
    }
  }

  test("foldLeft with Id consistent with Either foldLeft") {
    forAll { (x: EitherT[Id, String, Int], l: Long, f: (Long, Int) => Long) =>
      assert(x.foldLeft(l)(f) === (x.value.foldLeft(l)(f)))
    }
  }

  test("foldRight with Id consistent with Either foldRight") {
    forAll { (x: EitherT[Id, String, Int], l: Eval[Long], f: (Int, Eval[Long]) => Eval[Long]) =>
      assert(x.foldRight(l)(f) === (x.value.foldRight(l)(f)))
    }
  }

  test("collectRight with Option consistent with flattening a to[Option]") {
    forAll { (et: EitherT[Option, String, Int]) =>
      assert(et.collectRight === (et.to[Option].flatten))
    }
  }

  test("applyAlt with Id consistent with EitherT map") {
    forAll { (et: EitherT[Id, String, Int], f: Int => String) =>
      assert(et.applyAlt(EitherT.pure(f)) === (et.map(f)))
    }
  }

  test("merge with Id consistent with Either merge") {
    forAll { (x: EitherT[Id, Int, Int]) =>
      assert(x.merge === (x.value.merge))
    }
  }

  test("to consistent with toOption") {
    forAll { (x: EitherT[List, String, Int]) =>
      assert(x.to[Option] === (x.toOption.value))
    }
  }

  test("toEither consistent with toOption") {
    forAll { (x: EitherT[List, String, Int]) =>
      assert(x.value.map(_.toOption) === (x.toOption.value))
    }
  }

  test("ensure on left is identity") {
    forAll { (x: EitherT[Id, String, Int], s: String, p: Int => Boolean) =>
      if (x.isLeft) {
        assert(x.ensure(s)(p) === x)
      }
    }
  }

  test("ensure on right is identity if predicate satisfied") {
    forAll { (x: EitherT[Id, String, Int], s: String, p: Int => Boolean) =>
      if (x.isRight && p(x.getOrElse(0))) {
        assert(x.ensure(s)(p) === x)
      }
    }
  }

  test("ensure should fail if predicate not satisfied") {
    forAll { (x: EitherT[Id, String, Int], s: String, p: Int => Boolean) =>
      if (x.isRight && !p(x.getOrElse(0))) {
        assert(x.ensure(s)(p) === (EitherT.leftT[Id, Int](s)))
      }
    }
  }

  test("inference works in for-comprehension") {
    sealed abstract class AppError
    case object Error1 extends AppError
    case object Error2 extends AppError

    val either1: Id[Either[Error1.type, String]] = Right("hi").pure[Id]
    val either2: Id[Either[Error2.type, String]] = Right("bye").pure[Id]

    for {
      s1 <- EitherT(either1)
      s2 <- EitherT[Id, AppError, String](either2)
    } yield s1 ++ s2

    for {
      s1 <- EitherT(either1)
      s2 <- EitherT.right[AppError]("1".pure[Id])
    } yield s1 ++ s2

    for {
      s1 <- EitherT(either1)
      s2 <- EitherT.left[String](Error1.pure[Id])
    } yield s1 ++ s2

    for {
      s1 <- EitherT(either1)
      s2 <- EitherT.pure[Id, AppError]("1")
    } yield s1 ++ s2
  }

  test("leftFlatMap consistent with leftMap") {
    forAll { (eithert: EitherT[List, String, Int], f: String => String) =>
      assert(eithert.leftFlatMap(v => EitherT.left[Int](List(f(v)))) === (eithert.leftMap(f)))
    }
  }

  test("leftFlatMap consistent with swap and then flatMap") {
    forAll { (eithert: EitherT[List, String, Int], f: String => EitherT[List, String, Int]) =>
      assert(eithert.leftFlatMap(f) === (eithert.swap.flatMap(a => f(a).swap).swap))
    }
  }

  test("leftSemiflatMap consistent with leftMap") {
    forAll { (eithert: EitherT[List, String, Int], f: String => String) =>
      assert(eithert.leftSemiflatMap(v => List(f(v))) === (eithert.leftMap(f)))
    }
  }

  test("leftSemiflatmap consistent with swap and the semiflatMap") {
    forAll { (eithert: EitherT[List, String, Int], f: String => List[String]) =>
      assert(eithert.leftSemiflatMap(f) === (eithert.swap.semiflatMap(a => f(a)).swap))
    }
  }

  test("semiflatTap does not change the return value") {
    type TestEffect[A] = State[List[Int], A]
    forAll { (eithert: EitherT[TestEffect, String, Int], f: Int => TestEffect[Int], initial: List[Int]) =>
      assert(eithert.semiflatTap(v => f(v)).value.runA(initial) === (eithert.value.runA(initial)))
    }
  }

  test("semiflatTap runs the effect") {
    type TestEffect[A] = State[List[Int], A]
    forAll { (eithert: EitherT[TestEffect, String, Int], f: Int => TestEffect[Int], initial: List[Int]) =>
      assert(eithert.semiflatTap(v => f(v)).value.runS(initial) === (eithert.semiflatMap(f).value.runS(initial)))
    }
  }

  test("leftSemiflatTap does not change the return value") {
    type TestEffect[A] = State[List[Int], A]
    forAll { (eithert: EitherT[TestEffect, String, Int], f: String => TestEffect[Int], initial: List[Int]) =>
      assert(eithert.leftSemiflatTap(v => f(v)).value.runA(initial) === (eithert.value.runA(initial)))
    }
  }

  test("leftSemiflatTap runs the effect") {
    type TestEffect[A] = State[List[Int], A]
    forAll { (eithert: EitherT[TestEffect, String, Int], f: String => TestEffect[Int], initial: List[Int]) =>
      assert(
        eithert.leftSemiflatTap(v => f(v)).value.runS(initial) === (eithert.leftSemiflatMap(f).value.runS(initial))
      )
    }
  }

  test("leftSemiflatTap consistent with swap and the semiflatTap") {
    type TestEffect[A] = State[List[Int], A]
    forAll { (eithert: EitherT[TestEffect, String, Int], f: String => TestEffect[Int], initial: List[Int]) =>
      assert(
        eithert.leftSemiflatTap(v => f(v)).value.runA(initial) ===
          eithert.swap.semiflatTap(v => f(v)).swap.value.runA(initial)
      )
    }
  }

  test("biSemiflatTap does not change the return value") {
    type TestEffect[A] = State[List[Int], A]
    forAll {
      (eithert: EitherT[TestEffect, String, Int],
       fa: String => TestEffect[Int],
       fb: Int => TestEffect[Int],
       initial: List[Int]
      ) =>
        assert(eithert.biSemiflatTap(v => fa(v), v => fb(v)).value.runA(initial) === eithert.value.runA(initial))
    }
  }

  test("biSemiflatTap consistent with leftSemiflatTap and semiFlatTap") {
    type TestEffect[A] = State[List[Int], A]
    forAll {
      (eithert: EitherT[TestEffect, String, Int],
       fa: String => TestEffect[Int],
       fb: Int => TestEffect[Int],
       initial: List[Int]
      ) =>
        assert(
          eithert.biSemiflatTap(fa, fb).value.runS(initial) ===
            eithert.leftSemiflatTap(fa).semiflatTap(fb).value.runS(initial)
        )
    }
  }

  test("biSemiflatMap consistent with leftSemiflatMap and semiFlatmap") {
    forAll { (eithert: EitherT[List, String, Int], fa: String => List[Int], fb: Int => List[String]) =>
      assert(eithert.biSemiflatMap(fa, fb) === (eithert.leftSemiflatMap(fa).semiflatMap(fb)))
    }
  }

  test("biSemiflatMap consistent with leftSemiflatMap") {
    forAll { (eithert: EitherT[List, String, Int], fa: String => List[Int]) =>
      assert(eithert.biSemiflatMap(fa, List(_)) === (eithert.leftSemiflatMap(a => fa(a))))
    }
  }

  test("biSemiflatMap consistent with semiflatMap") {
    forAll { (eithert: EitherT[List, String, Int], fb: Int => List[String]) =>
      assert(eithert.biSemiflatMap(List(_), fb) === (eithert.semiflatMap(b => fb(b))))
    }
  }

  test("biflatMap consistent with flatMap") {
    forAll { (eithert: EitherT[List, String, Int], fb: Int => EitherT[List, String, Int]) =>
      val noChangeLeft = (s: String) => EitherT.left[Int](List(s))

      assert(eithert.biflatMap(noChangeLeft, fb) === (eithert.flatMap(fb)))
    }
  }

  test("biflatMap consistent with leftFlatMap") {
    forAll { (eithert: EitherT[List, String, Int], fa: String => EitherT[List, String, Int]) =>
      val noChangeRight = (i: Int) => EitherT.right[String](List(i))

      assert(eithert.biflatMap(fa, noChangeRight) === (eithert.leftFlatMap(fa)))
    }
  }

  test("biflatMap with Left and Right consistent with leftFlatMap and then flatMap") {
    forAll { (eithert: EitherT[List, String, Int], string: String, int: Int) =>
      val leftFun = (_: String) => EitherT.left[Int](List(string))
      val rightFun = (_: Int) => EitherT.right[String](List(int))

      assert(eithert.biflatMap(leftFun, rightFun) === (eithert.leftFlatMap(leftFun).flatMap(rightFun)))
    }
  }

  test("toIor is consistent with IorT(eitherT.value.map(Ior.fromEither))") {
    forAll { (eitherT: EitherT[List, String, Int]) =>
      assert(eitherT.toIor === IorT(eitherT.value.map(Ior.fromEither)))
    }
  }
}
