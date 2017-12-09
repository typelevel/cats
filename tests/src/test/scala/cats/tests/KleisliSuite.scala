package cats
package tests

import cats.Contravariant
import cats.arrow._
import cats.data.{Const, EitherT, Kleisli, Reader}
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import org.scalacheck.Arbitrary
import cats.kernel.laws.discipline.{MonoidTests, SemigroupTests}
import cats.laws.discipline.{SemigroupKTests, MonoidKTests}

class KleisliSuite extends CatsSuite {
  implicit def kleisliEq[F[_], A, B](implicit A: Arbitrary[A], FB: Eq[F[B]]): Eq[Kleisli[F, A, B]] =
    Eq.by[Kleisli[F, A, B], A => F[B]](_.run)

  implicit def readerEq[A, B](implicit A: Arbitrary[A], FB: Eq[Id[B]]): Eq[Reader[A, B]] =
    kleisliEq

  implicit val eitherTEq = EitherT.catsDataEqForEitherT[Kleisli[Option, Int, ?], Unit, Int]
  implicit val eitherTEq2 = EitherT.catsDataEqForEitherT[Reader[Int, ?], Unit, Int]

  implicit val iso = SemigroupalTests.Isomorphisms.invariant[Kleisli[Option, Int, ?]]
  implicit val iso2 = SemigroupalTests.Isomorphisms.invariant[Reader[Int, ?]]

  {
    implicit val instance: ApplicativeError[Kleisli[Option, Int, ?], Unit] = Kleisli.catsDataApplicativeErrorForKleisli[Option, Unit, Int](cats.instances.option.catsStdInstancesForOption)
    checkAll("Kleisli[Option, Int, Int] with Unit", ApplicativeErrorTests[Kleisli[Option, Int, ?], Unit](instance).applicativeError[Int, Int, Int])
    checkAll("ApplicativeError[Kleisli[Option, Int, Int], Unit]", SerializableTests.serializable(instance))
  }

  checkAll("Kleisli[Option, Int, Int] with Unit", MonadErrorTests[Kleisli[Option, Int, ?], Unit].monadError[Int, Int, Int])
  checkAll("MonadError[Kleisli[Option, Int, Int], Unit]", SerializableTests.serializable(MonadError[Kleisli[Option, Int, ?], Unit]))

  checkAll("Kleisli[Option, Int, Int]", SemigroupalTests[Kleisli[Option, Int, ?]].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Kleisli[Option, Int, ?]]", SerializableTests.serializable(Semigroupal[Kleisli[Option, Int, ?]]))

  checkAll("Kleisli[Option, Int, ?]", CommutativeMonadTests[Kleisli[Option, Int, ?]].commutativeMonad[Int, Int, Int])
  checkAll("CommutativeMonad[Kleisli[Option, Int, ?]]",SerializableTests.serializable(CommutativeMonad[Kleisli[Option, Int, ?]]))

  {
    implicit val catsDataArrowForKleisli = Kleisli.catsDataArrowForKleisli[List]
    checkAll("Kleisli[List, Int, Int]", ArrowTests[Kleisli[List, ?, ?]].arrow[Int, Int, Int, Int, Int, Int])
    checkAll("Arrow[Kleisli[List, ?, ?]]", SerializableTests.serializable(Arrow[Kleisli[List, ?, ?]]))
  }

  {
    implicit val catsDataCommutativeArrowForKleisli = Kleisli.catsDataCommutativeArrowForKleisli[Option]
    checkAll("Kleisli[Option, Int, Int]", CommutativeArrowTests[Kleisli[Option, ?, ?]].commutativeArrow[Int, Int, Int, Int, Int, Int])
    checkAll("CommutativeArrow[Kleisli[Option, ?, ?]]", SerializableTests.serializable(CommutativeArrow[Kleisli[Option, ?, ?]]))
  }

  {
    implicit val catsDataChoiceForKleisli = Kleisli.catsDataChoiceForKleisli[Option]
    checkAll("Kleisli[Option, Int, Int]", ChoiceTests[Kleisli[Option, ?, ?]].choice[Int, Int, Int, Int])
    checkAll("Choice[Kleisli[Option, ?, ?]]", SerializableTests.serializable(Choice[Kleisli[Option, ?, ?]]))
  }

  {
    implicit val catsDataMonadForKleisli = Kleisli.catsDataMonadForKleisli[Option, Int]
    checkAll("Kleisli[Option, Int, Int]", MonadTests[Kleisli[Option, Int, ?]].monad[Int, Int, Int])
    checkAll("Monad[Kleisli[Option, ?, ?], Int]", SerializableTests.serializable(Monad[Kleisli[Option, Int, ?]]))
  }

  {
    implicit val catsDataMonadForReader = Kleisli.catsDataMonadForKleisliId[Int]
    checkAll("Reader[Int, Int]", MonadTests[Reader[Int, ?]].monad[Int, Int, Int])
    checkAll("Monad[Reader[?, ?], Int]", SerializableTests.serializable(Monad[Reader[Int, ?]]))
  }

  {
    implicit val catsDataStrongForKleisli = Kleisli.catsDataStrongForKleisli[Option]
    checkAll("Kleisli[Option, Int, Int]", StrongTests[Kleisli[Option, ?, ?]].strong[Int, Int, Int, Int, Int, Int])
    checkAll("Strong[Kleisli[Option, ?, ?]]", SerializableTests.serializable(Strong[Kleisli[Option, ?, ?]]))
  }

  {
    implicit val catsDataFlatMapForKleisli = Kleisli.catsDataFlatMapForKleisli[Option, Int]
    checkAll("Kleisli[Option, Int, Int]", FlatMapTests[Kleisli[Option, Int, ?]].flatMap[Int, Int, Int])
    checkAll("FlatMap[Kleisli[Option, Int, ?]]", SerializableTests.serializable(FlatMap[Kleisli[Option, Int, ?]]))
  }

  {
    implicit val catsDataAlternativeForKleisli = Kleisli.catsDataAlternativeForKleisli[Option, Int]
    checkAll("Kleisli[Option, Int, Int]", AlternativeTests[Kleisli[Option, Int, ?]].alternative[Int, Int, Int])
    checkAll("Alternative[Kleisli[Option, Int, ?]]", SerializableTests.serializable(Alternative[Kleisli[Option, Int, ?]]))
  }

  {
    implicit val catsDataContravariantMonoidalForKleisli = Kleisli.catsDataContravariantMonoidalForKleisli[Const[String, ?], Int]
    checkAll("Kleisli[Const[String, ?], Int, Int]",
      ContravariantMonoidalTests[Kleisli[Const[String, ?], Int, ?]].contravariantMonoidal[Int, Int, Int])
    checkAll("ContravariantMonoidal[Kleisli[Option, Int, ?]]",
      SerializableTests.serializable(ContravariantMonoidal[Kleisli[Const[String, ?], Int, ?]]))
  }

  {
    implicit val catsDataApplicativeForKleisli = Kleisli.catsDataApplicativeForKleisli[Option, Int]
    checkAll("Kleisli[Option, Int, Int]", ApplicativeTests[Kleisli[Option, Int, ?]].applicative[Int, Int, Int])
    checkAll("Applicative[Kleisli[Option, Int, ?]]", SerializableTests.serializable(Applicative[Kleisli[Option, Int, ?]]))
  }

  {
    implicit val catsDataApplyForKleisli = Kleisli.catsDataApplyForKleisli[Option, Int]
    checkAll("Kleisli[Option, Int, Int]", ApplyTests[Kleisli[Option, Int, ?]].apply[Int, Int, Int])
    checkAll("Apply[Kleisli[Option, Int, ?]]", SerializableTests.serializable(Apply[Kleisli[Option, Int, ?]]))
  }

  {
    implicit val catsDataFunctorForKleisli = Kleisli.catsDataFunctorForKleisli[Option, Int]
    checkAll("Kleisli[Option, Int, Int]", FunctorTests[Kleisli[Option, Int, ?]].functor[Int, Int, Int])
    checkAll("Functor[Kleisli[Option, Int, ?]]", SerializableTests.serializable(Functor[Kleisli[Option, Int, ?]]))
  }

  {
    checkAll("Kleisli[Function0, Int, ?]", DistributiveTests[Kleisli[Function0, Int, ?]].distributive[Int, Int, Int, Option, Id])
    checkAll("Distributive[Kleisli[Option, Int, ?]]", SerializableTests.serializable(Distributive[Kleisli[Function0, Int, ?]]))
  }

  {
    implicit val catsDataMonoidForKleisli = Kleisli.catsDataMonoidForKleisli[Option, Int, String]
    checkAll("Kleisli[Option, Int, String]", MonoidTests[Kleisli[Option, Int, String]].monoid)
    checkAll("Monoid[Kleisli[Option, Int, String]]", SerializableTests.serializable(catsDataMonoidForKleisli))
  }

  {
    implicit val catsDataSemigroupForKleisli = Kleisli.catsDataSemigroupForKleisli[Option, Int, String]
    checkAll("Kleisli[Option, Int, String]", SemigroupTests[Kleisli[Option, Int, String]].semigroup)
    checkAll("Semigroup[Kleisli[Option, Int, String]]", SerializableTests.serializable(catsDataSemigroupForKleisli))
  }

  {
    implicit val catsDataMonoidKForKleisli = Kleisli.endoMonoidK[Option]
    checkAll("Kleisli[Option, Int, Int]", MonoidKTests[λ[α => Kleisli[Option, α, α]]].monoidK[Int])
    checkAll("MonoidK[λ[α => Kleisli[Option, α, α]]]", SerializableTests.serializable(catsDataMonoidKForKleisli))
  }

  {
    implicit val catsDataSemigroupKForKleisli = Kleisli.endoSemigroupK[Option]
    checkAll("Kleisli[Option, Int, Int]", SemigroupKTests[λ[α => Kleisli[Option, α, α]]].semigroupK[Int])
    checkAll("SemigroupK[λ[α => Kleisli[Option, α, α]]]", SerializableTests.serializable(catsDataSemigroupKForKleisli))
  }

  {
    implicit val semigroupk = Kleisli.catsDataSemigroupKForKleisli[Option, String]
    checkAll("Kleisli[Option, String, Int]", SemigroupKTests[Kleisli[Option, String, ?]].semigroupK[Int])
    checkAll("SemigroupK[Kleisli[Option, String, ?]]", SerializableTests.serializable(semigroupk))
  }

  {
    implicit val monoidk = Kleisli.catsDataMonoidKForKleisli[Option, String]
    checkAll("Kleisli[Option, String, Int]", MonoidKTests[Kleisli[Option, String, ?]].monoidK[Int])
    checkAll("MonoidK[Kleisli[Option, String, ?]]", SerializableTests.serializable(monoidk))
  }

  checkAll("Reader[Int, Int]", FunctorTests[Reader[Int, ?]].functor[Int, Int, Int])

  checkAll("Kleisli[Option, ?, Int]", ContravariantTests[Kleisli[Option, ?, Int]].contravariant[Int, Int, Int])
  checkAll("Contravariant[Kleisli[Option, ?, Int]]", SerializableTests.serializable(Contravariant[Kleisli[Option, ?, Int]]))

  test("local composes functions") {
    forAll { (f: Int => Option[String], g: Int => Int, i: Int) =>
      f(g(i)) should === (Kleisli.local[Option, String, Int](g)(Kleisli(f)).run(i))
    }
  }

  test("pure consistent with ask") {
    forAll { (i: Int) =>
      Kleisli.pure[Option, Int, Int](i).run(i) should === (Kleisli.ask[Option, Int].run(i))
    }
  }

  test("mapF") {
    forAll { (f: Kleisli[List, Int, Int], t: List[Int] => List[Int], i: Int) =>
      t(f.run(i)) should === (f.mapF(t).run(i))
    }
  }

  test("mapK") {
    val t: List ~> Option = λ[List ~> Option](_.headOption)
    forAll { (f: Kleisli[List, Int, Int], i: Int) =>
      t(f.run(i)) should === (f.mapK(t).run(i))
    }
  }

  test("flatMapF") {
    forAll { (f: Kleisli[List, Int, Int], t: Int => List[Int], i: Int) =>
      f.run(i).flatMap(t) should === (f.flatMapF(t).run(i))
    }
  }

  test("lower") {
    forAll { (f: Kleisli[List, Int, Int], i: Int) =>
      f.run(i) should === (f.lower.run(i).flatten)
    }
  }

  test("tap") {
    forAll { (f: Kleisli[List, Int, String], i: Int) =>
      f.run(i).as(i) should === (f.tap.run(i))
    }
  }

  test("tapWith") {
    forAll { (f: Kleisli[List, Int, String], g: (Int, String) => Boolean, i: Int) =>
      f.run(i).map(s => g(i, s)) should === (f.tapWith(g).run(i))
    }
  }

  test("toReader") {
    forAll { (f: Kleisli[List, Int, String], i: Int) =>
      f.run(i) should === (f.toReader.run(i))
    }
  }

  test("apply") {
    forAll { (f: Kleisli[List, Int, Int], i: Int) =>
      f.run(i) should === (f(i))
    }
  }

  test("traverse") {
    forAll { (f: Kleisli[List, Int, Int], i: Int) =>
      f.traverse(Some(i): Option[Int]) should === ((Some(i): Option[Int]).traverse(f(_)))
    }
  }

  test("lift") {
    val f = Kleisli { (x: Int) => (Some(x + 1): Option[Int]) }
    val l = f.lift[List]
    (List(1, 2, 3) >>= l.run) should === (List(Some(2), Some(3), Some(4)))
  }

  test("local") {
    case class Config(i: Int, s: String)

    val kint = Kleisli { (x: Int) => Option(x.toDouble) }
    val kconfig1 = kint.local[Config](_.i)
    val kconfig2 = Kleisli { (c: Config) => Option(c.i.toDouble) }

    val config = Config(0, "cats")
    kconfig1.run(config) should === (kconfig2.run(config))
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
    Monoid[Kleisli[Id, Int, String]]
    Arrow[Kleisli[Id, ?, ?]]
    CommutativeArrow[Kleisli[Id, ?, ?]]
    Choice[Kleisli[Id, ?, ?]]
    Strong[Kleisli[Id, ?, ?]]
    FlatMap[Kleisli[Id, Int, ?]]
    Semigroup[Kleisli[Id, Int, String]]

    // using Reader alias instead of Kleisli with Id as F
    Functor[Reader[Int, ?]]
    Apply[Reader[Int, ?]]
    Applicative[Reader[Int, ?]]
    Monad[Reader[Int, ?]]
    Monoid[Reader[Int, String]]
    Arrow[Reader[?, ?]]
    CommutativeArrow[Reader[?, ?]]
    Choice[Reader[?, ?]]
    Strong[Reader[?, ?]]
    FlatMap[Reader[Int, ?]]
    Semigroup[Reader[Int, String]]

    // using IntReader alias instead of Kleisli with Id as F and A as Int
    type IntReader[A] = Reader[Int, A]
    Functor[IntReader]
    Apply[IntReader]
    Applicative[IntReader]
    Monad[IntReader]
    Monoid[IntReader[String]]
    FlatMap[IntReader]
    Semigroup[IntReader[String]]

    ApplicativeError[Kleisli[cats.data.Validated[Unit, ?], Int, ?], Unit]
    ApplicativeError[Kleisli[Option, Int, ?], Unit]
    MonadError[Kleisli[Option, Int, ?], Unit]

    Distributive[Kleisli[Function0, Int, ?]]
  }
}
