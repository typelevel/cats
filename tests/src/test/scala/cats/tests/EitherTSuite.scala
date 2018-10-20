package cats
package tests

import cats.Bifunctor
import cats.data.EitherT

import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.kernel.laws.discipline.{EqTests, MonoidTests, OrderTests, PartialOrderTests, SemigroupTests}

class EitherTSuite extends CatsSuite {
  implicit val iso = SemigroupalTests.Isomorphisms
    .invariant[EitherT[ListWrapper, String, ?]](EitherT.catsDataFunctorForEitherT(ListWrapper.functor))

  checkAll("EitherT[Eval, String, ?]", DeferTests[EitherT[Eval, String, ?]].defer[Int])

  {
    checkAll("EitherT[Option, ListWrapper[String], ?]",
             SemigroupKTests[EitherT[Option, ListWrapper[String], ?]].semigroupK[Int])
    checkAll("SemigroupK[EitherT[Option, ListWrapper[String], ?]]",
             SerializableTests.serializable(SemigroupK[EitherT[Option, ListWrapper[String], ?]]))
  }

  {
    implicit val F = ListWrapper.order[Either[String, Int]]

    checkAll("EitherT[List, String, Int]", OrderTests[EitherT[ListWrapper, String, Int]].order)
    checkAll("Order[EitherT[List, String, Int]]",
             SerializableTests.serializable(Order[EitherT[ListWrapper, String, Int]]))
  }

  {
    //If a Functor for F is defined
    implicit val F = ListWrapper.functor

    checkAll("EitherT[ListWrapper, ?, ?]",
             BifunctorTests[EitherT[ListWrapper, ?, ?]].bifunctor[Int, Int, Int, String, String, String])
    checkAll("Bifunctor[EitherT[ListWrapper, ?, ?]]",
             SerializableTests.serializable(Bifunctor[EitherT[ListWrapper, ?, ?]]))
    checkAll("EitherT[ListWrapper, Int, ?]", FunctorTests[EitherT[ListWrapper, Int, ?]].functor[Int, Int, Int])
    checkAll("Functor[EitherT[ListWrapper, Int, ?]]",
             SerializableTests.serializable(Functor[EitherT[ListWrapper, Int, ?]]))
  }

  {
    //If a Traverse for F is defined
    implicit val F = ListWrapper.traverse

    checkAll("EitherT[ListWrapper, Int, ?]",
             TraverseTests[EitherT[ListWrapper, Int, ?]].traverse[Int, Int, Int, Int, Option, Option])
    checkAll("Traverse[EitherT[ListWrapper, Int, ?]]",
             SerializableTests.serializable(Traverse[EitherT[ListWrapper, Int, ?]]))
    checkAll("EitherT[ListWrapper, ?, ?]",
             BitraverseTests[EitherT[ListWrapper, ?, ?]].bitraverse[Option, Int, Int, Int, String, String, String])
    checkAll("Bitraverse[EitherT[ListWrapper, ?, ?]]",
             SerializableTests.serializable(Bitraverse[EitherT[ListWrapper, ?, ?]]))

  }

  {
    //if a Monad is defined

    implicit val F = ListWrapper.monad
    implicit val eq0 = EitherT.catsDataEqForEitherT[ListWrapper, String, Either[String, Int]]
    implicit val eq1 = EitherT.catsDataEqForEitherT[EitherT[ListWrapper, String, ?], String, Int](eq0)

    Functor[EitherT[ListWrapper, String, ?]]
    Applicative[EitherT[ListWrapper, String, ?]]
    Monad[EitherT[ListWrapper, String, ?]]

    checkAll("EitherT[ListWrapper, String, Int]",
             MonadErrorTests[EitherT[ListWrapper, String, ?], String].monadError[Int, Int, Int])
    checkAll("MonadError[EitherT[List, ?, ?]]",
             SerializableTests.serializable(MonadError[EitherT[ListWrapper, String, ?], String]))

  }

  {
    // if a MonadError is defined
    // Tests for catsDataMonadErrorFForEitherT instance, for recovery on errors of F.

    implicit val eq1 = EitherT.catsDataEqForEitherT[Option, String, Either[Unit, String]]
    implicit val eq2 = EitherT.catsDataEqForEitherT[EitherT[Option, String, ?], Unit, String](eq1)
    implicit val me = EitherT.catsDataMonadErrorFForEitherT[Option, Unit, String](catsStdInstancesForOption)

    Functor[EitherT[Option, String, ?]]
    Applicative[EitherT[Option, String, ?]]
    Monad[EitherT[Option, String, ?]]

    checkAll("EitherT[Option, String, String]",
             MonadErrorTests[EitherT[Option, String, ?], Unit].monadError[String, String, String])
    checkAll("MonadError[EitherT[Option, ?, ?]]",
             SerializableTests.serializable(MonadError[EitherT[Option, String, ?], Unit]))
  }

  {
    //if a Monad is defined
    implicit val F = ListWrapper.monad

    Functor[EitherT[ListWrapper, String, ?]]
    Applicative[EitherT[ListWrapper, String, ?]]
    Monad[EitherT[ListWrapper, String, ?]]

    checkAll("EitherT[ListWrapper, String, Int]", MonadTests[EitherT[ListWrapper, String, ?]].monad[Int, Int, Int])
    checkAll("Monad[EitherT[ListWrapper, String, ?]]",
             SerializableTests.serializable(Monad[EitherT[ListWrapper, String, ?]]))
  }

  {
    //If a foldable is defined
    implicit val F = ListWrapper.foldable

    checkAll("EitherT[ListWrapper, Int, ?]", FoldableTests[EitherT[ListWrapper, Int, ?]].foldable[Int, Int])
    checkAll("Foldable[EitherT[ListWrapper, Int, ?]]",
             SerializableTests.serializable(Foldable[EitherT[ListWrapper, Int, ?]]))
  }

  {
    implicit val F = ListWrapper.partialOrder[Either[String, Int]]

    checkAll("EitherT[ListWrapper, String, Int]", PartialOrderTests[EitherT[ListWrapper, String, Int]].partialOrder)
    checkAll("PartialOrder[EitherT[ListWrapper, String, Int]]",
             SerializableTests.serializable(PartialOrder[EitherT[ListWrapper, String, Int]]))
  }

  {
    implicit val F = ListWrapper.semigroup[Either[String, Int]]

    checkAll("EitherT[ListWrapper, String, Int]", SemigroupTests[EitherT[ListWrapper, String, Int]].semigroup)
    checkAll("Semigroup[EitherT[ListWrapper, String, Int]]",
             SerializableTests.serializable(Semigroup[EitherT[ListWrapper, String, Int]]))
  }

  {
    implicit val F = ListWrapper.monoid[Either[String, Int]]

    Semigroup[EitherT[ListWrapper, String, Int]]

    checkAll("EitherT[ListWrapper, String, Int]", MonoidTests[EitherT[ListWrapper, String, Int]].monoid)
    checkAll("Monoid[EitherT[ListWrapper, String, Int]]",
             SerializableTests.serializable(Monoid[EitherT[ListWrapper, String, Int]]))
  }

  {
    implicit val F = ListWrapper.eqv[Either[String, Int]]

    checkAll("EitherT[ListWrapper, String, Int]", EqTests[EitherT[ListWrapper, String, Int]].eqv)
    checkAll("Eq[EitherT[ListWrapper, String, Int]]",
             SerializableTests.serializable(Eq[EitherT[ListWrapper, String, Int]]))
  }

  test("toValidated") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      eithert.toValidated.map(_.toEither) should ===(eithert.value)
    }
  }

  test("toValidatedNel") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      eithert.toValidatedNel.map(_.toEither.leftMap(_.head)) should ===(eithert.value)
    }
  }

  test("toValidatedNec") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      eithert.toValidatedNec.map(_.toEither.leftMap(_.head)) should ===(eithert.value)
    }
  }

  test("toNested") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      eithert.toNested.value should ===(eithert.value)
    }
  }

  test("toNestedValidated") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      eithert.toNestedValidated.value should ===(eithert.value.map(_.toValidated))
    }
  }

  test("toNestedValidatedNel") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      eithert.toNestedValidatedNel.value should ===(eithert.value.map(_.toValidatedNel))
    }
  }

  test("toNestedValidatedNec") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      eithert.toNestedValidatedNec.value should ===(eithert.value.map(_.toValidatedNec))
    }
  }

  test("withValidated") {
    forAll { (eithert: EitherT[List, String, Int], f: String => Char, g: Int => Double) =>
      eithert.withValidated(_.bimap(f, g)) should ===(eithert.bimap(f, g))
    }
  }

  test("fromEither") {
    forAll { (either: Either[String, Int]) =>
      Some(either.isLeft) should ===(EitherT.fromEither[Option](either).isLeft)
    }
  }

  test("fromOption isLeft consistent with Option.isEmpty") {
    forAll { (o: Option[Int], s: String) =>
      EitherT.fromOption[Id](o, s).isLeft should ===(o.isEmpty)
    }
  }

  test("cond consistent with Either.cond") {
    forAll { (cond: Boolean, s: String, i: Int) =>
      EitherT.cond[Id](cond, s, i).value should ===(Either.cond(cond, s, i))
    }
  }

  test("isLeft negation of isRight") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      eithert.isLeft should ===(eithert.isRight.map(!_))
    }
  }

  test("double swap is noop") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      eithert.swap.swap should ===(eithert)
    }
  }

  test("swap negates isRight") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      eithert.swap.isRight should ===(eithert.isRight.map(!_))
    }
  }

  test("toOption on Right returns Some") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      eithert.toOption.isDefined should ===(eithert.isRight)
    }
  }

  test("toEither preserves isRight") {
    forAll { (eithert: EitherT[List, String, Int]) =>
      eithert.value.map(_.isRight) should ===(eithert.isRight)
    }
  }

  test("recover recovers handled values") {
    val eithert = EitherT.leftT[Id, Int]("eithert")
    eithert.recover { case "eithert" => 5 }.isRight should ===(true)
  }

  test("recover ignores unhandled values") {
    val eithert = EitherT.leftT[Id, Int]("eithert")
    eithert.recover { case "noteithert" => 5 } should ===(eithert)
  }

  test("recover ignores the right side") {
    val eithert = EitherT.pure[Id, String](10)
    eithert.recover { case "eithert" => 5 } should ===(eithert)
  }

  test("recoverWith recovers handled values") {
    val eithert = EitherT.leftT[Id, Int]("eithert")
    eithert.recoverWith { case "eithert" => EitherT.pure[Id, String](5) }.isRight should ===(true)
  }

  test("recoverWith ignores unhandled values") {
    val eithert = EitherT.leftT[Id, Int]("eithert")
    eithert.recoverWith { case "noteithert" => EitherT.pure[Id, String](5) } should ===(eithert)
  }

  test("transform consistent with value.map") {
    forAll { (eithert: EitherT[List, String, Int], f: Either[String, Int] => Either[Long, Double]) =>
      eithert.transform(f) should ===(EitherT(eithert.value.map(f)))
    }
  }

  test("mapK consistent with f(value)+pure") {
    val f: List ~> Option = λ[List ~> Option](_.headOption)
    forAll { (eithert: EitherT[List, String, Int]) =>
      eithert.mapK(f) should ===(EitherT(f(eithert.value)))
    }
  }

  test("semiflatMap consistent with value.flatMap+f+pure") {
    forAll { (eithert: EitherT[List, String, Int], f: Int => List[String]) =>
      eithert.semiflatMap(f) should ===(EitherT(eithert.value.flatMap {
        case l @ Left(_) => List(l.asInstanceOf[Either[String, String]])
        case Right(b)    => f(b).map(Right(_))
      }))
    }
  }

  test("subflatMap consistent with value.map+flatMap") {
    forAll { (eithert: EitherT[List, String, Int], f: Int => Either[String, Double]) =>
      eithert.subflatMap(f) should ===(EitherT(eithert.value.map(_.flatMap(f))))
    }
  }

  test("flatMap and flatMapF consistent") {
    forAll { (eithert: EitherT[List, String, Int], f: Int => EitherT[List, String, Int]) =>
      eithert.flatMap(f) should ===(eithert.flatMapF(f(_).value))
    }
  }

  test("fold with Id consistent with Either fold") {
    forAll { (eithert: EitherT[Id, String, Int], f: String => Long, g: Int => Long) =>
      eithert.fold(f, g) should ===(eithert.value.fold(f, g))
    }
  }

  test("valueOr with Id consistent with Either valueOr") {
    forAll { (eithert: EitherT[Id, String, Int], f: String => Int) =>
      eithert.valueOr(f) should ===(eithert.value.valueOr(f))
    }
  }

  test("valueOrF with Id consistent with Either valueOr") {
    forAll { (eithert: EitherT[Id, String, Int], f: String => Int) =>
      eithert.valueOrF(f) should ===(eithert.value.valueOr(f))
    }
  }

  test("getOrElse with Id consistent with Either getOrElse") {
    forAll { (eithert: EitherT[Id, String, Int], i: Int) =>
      eithert.getOrElse(i) should ===(eithert.value.getOrElse(i))
    }
  }

  test("getOrElseF with Id consistent with Either getOrElse") {
    forAll { (eithert: EitherT[Id, String, Int], i: Int) =>
      eithert.getOrElseF(i) should ===(eithert.value.getOrElse(i))
    }
  }

  test("orElse with Id consistent with Either orElse") {
    forAll { (eithert: EitherT[Id, String, Int], fallback: EitherT[Id, String, Int]) =>
      eithert.orElse(fallback).value should ===(eithert.value.orElse(fallback.value))
    }
  }

  test("orElse evaluates effect only once") {
    forAll { (either: Either[String, Int], fallback: EitherT[Eval, String, Int]) =>
      var evals = 0
      val eithert = EitherT(Eval.always { evals += 1; either }).orElse(fallback)
      eithert.value.value
      evals should ===(1)
    }
  }

  test("forall with Id consistent with Either forall") {
    forAll { (eithert: EitherT[Id, String, Int], f: Int => Boolean) =>
      eithert.forall(f) should ===(eithert.value.forall(f))
    }
  }

  test("exists with Id consistent with Either exists") {
    forAll { (eithert: EitherT[Id, String, Int], f: Int => Boolean) =>
      eithert.exists(f) should ===(eithert.value.exists(f))
    }
  }

  test("leftMap with Id consistent with Either leftMap") {
    forAll { (eithert: EitherT[Id, String, Int], f: String => Long) =>
      eithert.leftMap(f).value should ===(eithert.value.leftMap(f))
    }
  }

  test("compare with Id consistent with Either compare") {
    forAll { (x: EitherT[Id, String, Int], y: EitherT[Id, String, Int]) =>
      x.compare(y) should ===(x.value.compare(y.value))
    }
  }

  test("=== with Id consistent with Either ===") {
    forAll { (x: EitherT[Id, String, Int], y: EitherT[Id, String, Int]) =>
      x === y should ===(x.value === y.value)
    }
  }

  test("traverse with Id consistent with Either traverse") {
    forAll { (x: EitherT[Id, String, Int], f: Int => Option[Long]) =>
      val e: Either[String, Int] = x.value
      x.traverse(f).map(_.value) should ===(e.traverse(f))
    }
  }

  test("foldLeft with Id consistent with Either foldLeft") {
    forAll { (x: EitherT[Id, String, Int], l: Long, f: (Long, Int) => Long) =>
      x.foldLeft(l)(f) should ===(x.value.foldLeft(l)(f))
    }
  }

  test("foldRight with Id consistent with Either foldRight") {
    forAll { (x: EitherT[Id, String, Int], l: Eval[Long], f: (Int, Eval[Long]) => Eval[Long]) =>
      x.foldRight(l)(f) should ===(x.value.foldRight(l)(f))
    }
  }

  test("collectRight with Option consistent with flattening a to[Option]") {
    forAll { (et: EitherT[Option, String, Int]) =>
      et.collectRight should ===(et.to[Option].flatten)
    }
  }

  test("applyAlt with Id consistent with EitherT map") {
    forAll { (et: EitherT[Id, String, Int], f: Int => String) =>
      et.applyAlt(EitherT.pure(f)) should ===(et.map(f))
    }
  }

  test("merge with Id consistent with Either merge") {
    forAll { (x: EitherT[Id, Int, Int]) =>
      x.merge should ===(x.value.merge)
    }
  }

  test("to consistent with toOption") {
    forAll { (x: EitherT[List, String, Int]) =>
      x.to[Option] should ===(x.toOption.value)
    }
  }

  test("toEither consistent with toOption") {
    forAll { (x: EitherT[List, String, Int]) =>
      x.value.map(_.right.toOption) should ===(x.toOption.value)
    }
  }

  test("ensure on left is identity") {
    forAll { (x: EitherT[Id, String, Int], s: String, p: Int => Boolean) =>
      if (x.isLeft) {
        x.ensure(s)(p) should ===(x)
      }
    }
  }

  test("ensure on right is identity if predicate satisfied") {
    forAll { (x: EitherT[Id, String, Int], s: String, p: Int => Boolean) =>
      if (x.isRight && p(x.getOrElse(0))) {
        x.ensure(s)(p) should ===(x)
      }
    }
  }

  test("ensure should fail if predicate not satisfied") {
    forAll { (x: EitherT[Id, String, Int], s: String, p: Int => Boolean) =>
      if (x.isRight && !p(x.getOrElse(0))) {
        x.ensure(s)(p) should ===(EitherT.leftT[Id, Int](s))
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
      eithert.leftFlatMap(v => EitherT.left[Int](List(f(v)))) should ===(eithert.leftMap(f))
    }
  }

  test("leftFlatMap consistent with swap and then flatMap") {
    forAll { (eithert: EitherT[List, String, Int], f: String => EitherT[List, String, Int]) =>
      eithert.leftFlatMap(f) should ===(eithert.swap.flatMap(a => f(a).swap).swap)
    }
  }

  test("leftSemiflatMap consistent with leftMap") {
    forAll { (eithert: EitherT[List, String, Int], f: String => String) =>
      eithert.leftSemiflatMap(v => List(f(v))) should ===(eithert.leftMap(f))
    }
  }

  test("leftSemiflatmap consistent with swap and the semiflatMap") {
    forAll { (eithert: EitherT[List, String, Int], f: String => List[String]) =>
      eithert.leftSemiflatMap(f) should ===(eithert.swap.semiflatMap(a => f(a)).swap)
    }
  }

  test("biSemiflatMap consistent with leftSemiflatMap and semiFlatmap") {
    forAll { (eithert: EitherT[List, String, Int], fa: String => List[Int], fb: Int => List[String]) =>
      eithert.biSemiflatMap(fa, fb) should ===(eithert.leftSemiflatMap(fa).semiflatMap(fb))
    }
  }

  test("biSemiflatMap consistent with leftSemiflatMap") {
    forAll { (eithert: EitherT[List, String, Int], fa: String => List[Int]) =>
      eithert.biSemiflatMap(fa, List(_)) should ===(eithert.leftSemiflatMap(a => fa(a)))
    }
  }

  test("biSemiflatMap consistent with semiflatMap") {
    forAll { (eithert: EitherT[List, String, Int], fb: Int => List[String]) =>
      eithert.biSemiflatMap(List(_), fb) should ===(eithert.semiflatMap(b => fb(b)))
    }
  }

  test("biFlatMap consistent with flatMap") {
    forAll { (eithert: EitherT[List, String, Int], fb: Int => EitherT[List, String, Int]) =>
      val noChangeLeft = (s: String) => EitherT.left[Int](List(s))

      eithert.biFlatMap(noChangeLeft, fb) should ===(eithert.flatMap(fb))
    }
  }

  test("biFlatMap consistent with leftFlatMap") {
    forAll { (eithert: EitherT[List, String, Int], fa: String => EitherT[List, String, Int]) =>
      val noChangeRight = (i: Int) => EitherT.right[String](List(i))

      eithert.biFlatMap(fa, noChangeRight) should ===(eithert.leftFlatMap(fa))
    }
  }

  test("biFlatMap with Left and Right consistent with leftFlatMap and then flatMap") {
    forAll { (eithert: EitherT[List, String, Int], string: String, int: Int) =>
      val leftFun = (_: String) => EitherT.left[Int](List(string))
      val rightFun = (_: Int) => EitherT.right[String](List(int))

      eithert.biFlatMap(leftFun, rightFun) should ===(eithert.leftFlatMap(leftFun).flatMap(rightFun))
    }
  }

}
