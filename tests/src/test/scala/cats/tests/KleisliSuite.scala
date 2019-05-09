package cats
package tests

import cats.Contravariant
import cats.arrow._
import cats.data.{Const, EitherT, Kleisli, Reader, ReaderT}
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import cats.kernel.laws.discipline.{MonoidTests, SemigroupTests}
import cats.laws.discipline.{DeferTests, MonoidKTests, SemigroupKTests}
import cats.platform.Platform
import Helpers.CSemi

class KleisliSuite extends CatsSuite {
  implicit def kleisliEq[F[_], A, B](implicit ev: Eq[A => F[B]]): Eq[Kleisli[F, A, B]] =
    Eq.by[Kleisli[F, A, B], A => F[B]](_.run)

  implicit def readerEq[A, B](implicit ev: Eq[A => B]): Eq[Reader[A, B]] =
    kleisliEq

  implicit val eitherTEq = EitherT.catsDataEqForEitherT[Kleisli[Option, MiniInt, ?], Unit, Int]
  implicit val eitherTEq2 = EitherT.catsDataEqForEitherT[Reader[MiniInt, ?], Unit, Int]

  implicit val iso = SemigroupalTests.Isomorphisms.invariant[Kleisli[Option, Int, ?]]
  implicit val iso2 = SemigroupalTests.Isomorphisms.invariant[Reader[Int, ?]]

  {
    implicit val instance: ApplicativeError[Kleisli[Option, MiniInt, ?], Unit] =
      Kleisli.catsDataApplicativeErrorForKleisli[Option, Unit, MiniInt](cats.instances.option.catsStdInstancesForOption)
    checkAll("Kleisli[Option, MinInt, ?] with Unit",
             ApplicativeErrorTests[Kleisli[Option, MiniInt, ?], Unit](instance).applicativeError[Int, Int, Int])
    checkAll("ApplicativeError[Kleisli[Option, Int, Int], Unit]", SerializableTests.serializable(instance))
  }

  checkAll("Kleisli[Eval, MiniInt, ?]", DeferTests[Kleisli[Eval, MiniInt, ?]].defer[Int])
  checkAll("Kleisli[Option, MiniInt, ?] with Unit",
           MonadErrorTests[Kleisli[Option, MiniInt, ?], Unit].monadError[Int, Int, Int])
  checkAll("MonadError[Kleisli[Option, Int, Int], Unit]",
           SerializableTests.serializable(MonadError[Kleisli[Option, Int, ?], Unit]))

  checkAll("Kleisli[Option, MiniInt, ?]", SemigroupalTests[Kleisli[Option, MiniInt, ?]].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Kleisli[Option, Int, ?]]", SerializableTests.serializable(Semigroupal[Kleisli[Option, Int, ?]]))

  checkAll("Kleisli[(CSemi, ?), MiniInt, ?]",
           CommutativeFlatMapTests[Kleisli[(CSemi, ?), MiniInt, ?]].commutativeFlatMap[Int, Int, Int])
  checkAll("CommutativeFlatMap[Kleisli[(CSemi, ?), Int, ?]]",
           SerializableTests.serializable(CommutativeFlatMap[Kleisli[(CSemi, ?), Int, ?]]))

  checkAll("Kleisli[Option, MiniInt, ?]",
           CommutativeMonadTests[Kleisli[Option, MiniInt, ?]].commutativeMonad[Int, Int, Int])
  checkAll("CommutativeMonad[Kleisli[Option, Int, ?]]",
           SerializableTests.serializable(CommutativeMonad[Kleisli[Option, Int, ?]]))

  checkAll("Kleisli[Id, MiniInt, ?]", CommutativeMonadTests[Kleisli[Id, MiniInt, ?]].commutativeMonad[Int, Int, Int])
  checkAll("CommutativeMonad[Kleisli[Id, Int, ?]]",
           SerializableTests.serializable(CommutativeMonad[Kleisli[Id, Int, ?]]))

  checkAll("Kleisli[List, ?, ?]",
           ArrowTests[Kleisli[List, ?, ?]].arrow[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, Boolean])
  checkAll("Arrow[Kleisli[List, ?, ?]]", SerializableTests.serializable(Arrow[Kleisli[List, ?, ?]]))

  checkAll("Kleisli[List, ?, ?]",
           ArrowChoiceTests[Kleisli[List, ?, ?]].arrowChoice[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, Boolean])
  checkAll("ArrowChoice[Kleisli[List, ?, ?]]", SerializableTests.serializable(ArrowChoice[Kleisli[List, ?, ?]]))

  checkAll("Kleisli[Option, Int, Int]",
           CommutativeArrowTests[Kleisli[Option, ?, ?]]
             .commutativeArrow[MiniInt, MiniInt, MiniInt, MiniInt, MiniInt, Boolean])
  checkAll("CommutativeArrow[Kleisli[Option, ?, ?]]",
           SerializableTests.serializable(CommutativeArrow[Kleisli[Option, ?, ?]]))

  checkAll("Kleisli[Option, ?, ?]", ChoiceTests[Kleisli[Option, ?, ?]].choice[MiniInt, Boolean, Int, Int])
  checkAll("Choice[Kleisli[Option, ?, ?]]", SerializableTests.serializable(Choice[Kleisli[Option, ?, ?]]))

  checkAll("Kleisli[Option, MiniInt, ?]", MonadTests[Kleisli[Option, MiniInt, ?]].monad[Int, Int, Int])
  checkAll("Monad[Kleisli[Option, ?, ?], Int]", SerializableTests.serializable(Monad[Kleisli[Option, Int, ?]]))

  checkAll("Reader[MiniInt, ?]", MonadTests[Reader[MiniInt, ?]].monad[Int, Int, Int])
  checkAll("Monad[Reader[?, ?], Int]", SerializableTests.serializable(Monad[Reader[Int, ?]]))

  checkAll("Kleisli[Option, ?, ?]",
           StrongTests[Kleisli[Option, ?, ?]].strong[MiniInt, Boolean, Boolean, Boolean, Boolean, Int])
  checkAll("Strong[Kleisli[Option, ?, ?]]", SerializableTests.serializable(Strong[Kleisli[Option, ?, ?]]))

  checkAll("Kleisli[Option, MiniInt, Int]", FlatMapTests[Kleisli[Option, MiniInt, ?]].flatMap[Int, Int, Int])
  checkAll("FlatMap[Kleisli[Option, Int, ?]]", SerializableTests.serializable(FlatMap[Kleisli[Option, Int, ?]]))

  checkAll("Kleisli[Option, MiniInt, ?]", AlternativeTests[Kleisli[Option, MiniInt, ?]].alternative[Int, Int, Int])
  checkAll("Alternative[Kleisli[Option, Int, ?]]", SerializableTests.serializable(Alternative[Kleisli[Option, Int, ?]]))

  checkAll("Kleisli[Const[String, ?], MiniInt, ?]",
           ContravariantMonoidalTests[Kleisli[Const[String, ?], MiniInt, ?]].contravariantMonoidal[Int, Int, Int])
  checkAll("ContravariantMonoidal[Kleisli[Option, Int, ?]]",
           SerializableTests.serializable(ContravariantMonoidal[Kleisli[Const[String, ?], Int, ?]]))

  checkAll("Kleisli[Option, MiniInt, Int]", ApplicativeTests[Kleisli[Option, MiniInt, ?]].applicative[Int, Int, Int])
  checkAll("Applicative[Kleisli[Option, Int, ?]]", SerializableTests.serializable(Applicative[Kleisli[Option, Int, ?]]))

  checkAll("Kleisli[Option, MiniInt, Int]", ApplyTests[Kleisli[Option, MiniInt, ?]].apply[Int, Int, Int])
  checkAll("Apply[Kleisli[Option, Int, ?]]", SerializableTests.serializable(Apply[Kleisli[Option, Int, ?]]))

  checkAll("Kleisli[Option, MiniInt, Int]", FunctorTests[Kleisli[Option, MiniInt, ?]].functor[Int, Int, Int])
  checkAll("Functor[Kleisli[Option, Int, ?]]", SerializableTests.serializable(Functor[Kleisli[Option, Int, ?]]))

  {
    implicit val FF = ListWrapper.functorFilter

    checkAll("Kleisli[ListWrapper, MiniInt, ?]",
             FunctorFilterTests[Kleisli[ListWrapper, MiniInt, ?]].functorFilter[Int, Int, Int])
    checkAll("FunctorFilter[Kleisli[ListWrapper, MiniInt, ?]]",
             SerializableTests.serializable(FunctorFilter[Kleisli[ListWrapper, MiniInt, ?]]))

    FunctorFilter[ReaderT[ListWrapper, Int, ?]]
  }

  checkAll("Kleisli[Function0, MiniInt, ?]",
           DistributiveTests[Kleisli[Function0, MiniInt, ?]].distributive[Int, Int, Int, Option, Id])
  checkAll("Distributive[Kleisli[Function0, Int, ?]]",
           SerializableTests.serializable(Distributive[Kleisli[Function0, Int, ?]]))

  checkAll("Kleisli[Option, MiniInt, String]", MonoidTests[Kleisli[Option, MiniInt, String]].monoid)
  checkAll("Monoid[Kleisli[Option, Int, String]]", SerializableTests.serializable(Monoid[Kleisli[Option, Int, String]]))

  checkAll("Kleisli[Option, MiniInt, String]", SemigroupTests[Kleisli[Option, MiniInt, String]].semigroup)
  checkAll("Semigroup[Kleisli[Option, Int, String]]",
           SerializableTests.serializable(Semigroup[Kleisli[Option, Int, String]]))

  {
    implicit val catsDataMonoidKForKleisli = Kleisli.endoMonoidK[Option]
    checkAll("Kleisli[Option, MiniInt, MiniInt]", MonoidKTests[λ[α => Kleisli[Option, α, α]]].monoidK[MiniInt])
    checkAll("MonoidK[λ[α => Kleisli[Option, α, α]]]", SerializableTests.serializable(catsDataMonoidKForKleisli))
  }

  {
    implicit val catsDataSemigroupKForKleisli = Kleisli.endoSemigroupK[Option]
    checkAll("Kleisli[Option, MiniInt, MiniInt]", SemigroupKTests[λ[α => Kleisli[Option, α, α]]].semigroupK[MiniInt])
    checkAll("SemigroupK[λ[α => Kleisli[Option, α, α]]]",
             SerializableTests.serializable(SemigroupK[λ[α => Kleisli[Option, α, α]]]))
  }

  checkAll("Kleisli[Option, MiniInt, Int]", SemigroupKTests[Kleisli[Option, MiniInt, ?]].semigroupK[Int])
  checkAll("SemigroupK[Kleisli[Option, String, ?]]",
           SerializableTests.serializable(SemigroupK[Kleisli[Option, String, ?]]))

  checkAll("Kleisli[Option, MiniInt, ?]", MonoidKTests[Kleisli[Option, MiniInt, ?]].monoidK[Int])
  checkAll("MonoidK[Kleisli[Option, String, ?]]", SerializableTests.serializable(MonoidK[Kleisli[Option, String, ?]]))

  checkAll("Reader[MiniInt, Int]", FunctorTests[Reader[MiniInt, ?]].functor[Int, Int, Int])

  checkAll("Kleisli[Option, ?, Int]", ContravariantTests[Kleisli[Option, ?, Int]].contravariant[MiniInt, Int, Boolean])
  checkAll("Contravariant[Kleisli[Option, ?, Int]]",
           SerializableTests.serializable(Contravariant[Kleisli[Option, ?, Int]]))

  test("Functor[Kleisli[F, Int, ?]] is not ambiguous when an ApplicativeError and a FlatMap are in scope for F") {
    def shouldCompile1[F[_]: ApplicativeError[?[_], E]: FlatMap, E]: Functor[Kleisli[F, Int, ?]] =
      Functor[Kleisli[F, Int, ?]]
  }

  test("local composes functions") {
    forAll { (f: Int => Option[String], g: Int => Int, i: Int) =>
      f(g(i)) should ===(Kleisli.local[Option, String, Int](g)(Kleisli(f)).run(i))
    }
  }

  test("pure consistent with ask") {
    forAll { (i: Int) =>
      Kleisli.pure[Option, Int, Int](i).run(i) should ===(Kleisli.ask[Option, Int].run(i))
    }
  }

  test("mapF") {
    forAll { (f: Kleisli[List, Int, Int], t: List[Int] => List[Int], i: Int) =>
      t(f.run(i)) should ===(f.mapF(t).run(i))
    }
  }

  test("mapK") {
    val t: List ~> Option = λ[List ~> Option](_.headOption)
    forAll { (f: Kleisli[List, Int, Int], i: Int) =>
      t(f.run(i)) should ===(f.mapK(t).run(i))
    }
  }

  test("liftFunctionK consistent with mapK") {
    val t: List ~> Option = λ[List ~> Option](_.headOption)
    forAll { (f: Kleisli[List, Int, Int], i: Int) =>
      (f.mapK(t).run(i)) should ===(Kleisli.liftFunctionK(t)(f).run(i))
    }
  }

  test("flatMapF") {
    forAll { (f: Kleisli[List, Int, Int], t: Int => List[Int], i: Int) =>
      f.run(i).flatMap(t) should ===(f.flatMapF(t).run(i))
    }
  }

  test("lower") {
    forAll { (f: Kleisli[List, Int, Int], i: Int) =>
      f.run(i) should ===(f.lower.run(i).flatten)
    }
  }

  test("tap") {
    forAll { (f: Kleisli[List, Int, String], i: Int) =>
      f.run(i).as(i) should ===(f.tap.run(i))
    }
  }

  test("tapWith") {
    forAll { (f: Kleisli[List, Int, String], g: (Int, String) => Boolean, i: Int) =>
      f.run(i).map(s => g(i, s)) should ===(f.tapWith(g).run(i))
    }
  }

  test("toReader") {
    forAll { (f: Kleisli[List, Int, String], i: Int) =>
      f.run(i) should ===(f.toReader.run(i))
    }
  }

  test("tapWithF") {
    forAll { (f: Kleisli[List, Int, String], g: (Int, String) => List[Boolean], i: Int) =>
      f.run(i).flatMap(s => g(i, s)) should ===(f.tapWithF(g).run(i))
    }
  }

  test("apply") {
    forAll { (f: Kleisli[List, Int, Int], i: Int) =>
      f.run(i) should ===(f(i))
    }
  }

  test("traverse") {
    forAll { (f: Kleisli[List, Int, Int], i: Int) =>
      f.traverse(Some(i): Option[Int]) should ===((Some(i): Option[Int]).traverse(f(_)))
    }
  }

  test("lift") {
    val f = Kleisli { (x: Int) =>
      (Some(x + 1): Option[Int])
    }
    val l = f.lift[List]
    (List(1, 2, 3) >>= l.run) should ===(List(Some(2), Some(3), Some(4)))
  }

  test("local") {
    case class Config(i: Int, s: String)

    val kint = Kleisli { (x: Int) =>
      Option(x.toDouble)
    }
    val kconfig1 = kint.local[Config](_.i)
    val kconfig2 = Kleisli { (c: Config) =>
      Option(c.i.toDouble)
    }

    val config = Config(0, "cats")
    kconfig1.run(config) should ===(kconfig2.run(config))
  }

  test("flatMap is stack safe on repeated left binds when F is") {
    val unit = Kleisli.pure[Eval, Unit, Unit](())
    val count = if (Platform.isJvm) 10000 else 100
    val result = (0 until count).foldLeft(unit) { (acc, _) =>
      acc.flatMap(_ => unit)
    }
    result.run(()).value
  }

  test("flatMap is stack safe on repeated right binds when F is") {
    val unit = Kleisli.pure[Eval, Unit, Unit](())
    val count = if (Platform.isJvm) 10000 else 100
    val result = (0 until count).foldLeft(unit) { (acc, _) =>
      unit.flatMap(_ => acc)
    }
    result.run(()).value
  }

  /**
   * Testing that implicit resolution works. If it compiles, the "test" passes.
   */
  object ImplicitResolution {
    // F is List
    Functor[Kleisli[List, Int, ?]]
    Apply[Kleisli[List, Int, ?]]
    Applicative[Kleisli[List, Int, ?]]
    Alternative[Kleisli[List, Int, ?]]
    Monad[Kleisli[List, Int, ?]]
    Monoid[Kleisli[List, Int, String]]
    MonoidK[Kleisli[List, Int, ?]]
    Arrow[Kleisli[List, ?, ?]]
    Choice[Kleisli[List, ?, ?]]
    Strong[Kleisli[List, ?, ?]]
    FlatMap[Kleisli[List, Int, ?]]
    Semigroup[Kleisli[List, Int, String]]
    SemigroupK[Kleisli[List, Int, ?]]

    // F is Id
    Functor[Kleisli[Id, Int, ?]]
    Apply[Kleisli[Id, Int, ?]]
    Applicative[Kleisli[Id, Int, ?]]
    Monad[Kleisli[Id, Int, ?]]
    CommutativeMonad[Kleisli[Id, Int, ?]]
    Monoid[Kleisli[Id, Int, String]]
    Arrow[Kleisli[Id, ?, ?]]
    CommutativeArrow[Kleisli[Id, ?, ?]]
    Choice[Kleisli[Id, ?, ?]]
    Strong[Kleisli[Id, ?, ?]]
    CommutativeFlatMap[Kleisli[Id, Int, ?]]
    Semigroup[Kleisli[Id, Int, String]]

    // using Reader alias instead of Kleisli with Id as F
    Functor[Reader[Int, ?]]
    Apply[Reader[Int, ?]]
    Applicative[Reader[Int, ?]]
    Monad[Reader[Int, ?]]
    CommutativeMonad[Reader[Int, ?]]
    Monoid[Reader[Int, String]]
    Arrow[Reader[?, ?]]
    CommutativeArrow[Reader[?, ?]]
    Choice[Reader[?, ?]]
    Strong[Reader[?, ?]]
    CommutativeFlatMap[Reader[Int, ?]]
    Semigroup[Reader[Int, String]]

    // using IntReader alias instead of Kleisli with Id as F and A as Int
    type IntReader[A] = Reader[Int, A]
    Functor[IntReader]
    Apply[IntReader]
    Applicative[IntReader]
    Monad[IntReader]
    CommutativeMonad[IntReader]
    Monoid[IntReader[String]]
    FlatMap[IntReader]
    CommutativeFlatMap[IntReader]
    Semigroup[IntReader[String]]

    ApplicativeError[Kleisli[cats.data.Validated[Unit, ?], Int, ?], Unit]
    ApplicativeError[Kleisli[Option, Int, ?], Unit]
    MonadError[Kleisli[Option, Int, ?], Unit]

    Distributive[Kleisli[Function0, Int, ?]]
  }
}
