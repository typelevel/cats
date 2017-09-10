package cats
package tests

import cats.arrow.Arrow
import cats.data.{State, StateT, IndexedStateT, EitherT}
import cats.functor.{Contravariant, Bifunctor, Profunctor, Strong}
import cats.kernel.instances.tuple._
import cats.laws.discipline._
import cats.laws.discipline.eq._
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary

class IndexedStateTTests extends CatsSuite {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    checkConfiguration.copy(sizeRange = 5)

  import IndexedStateTTests._

  test("basic state usage"){
    add1.run(1).value should === (2 -> 1)
  }

  test("basic IndexedStateT usage") {
    val listHead: IndexedStateT[Id, List[Int], Option[Int], Unit] = IndexedStateT.modify(_.headOption)
    val getOrElse: IndexedStateT[Id, Option[Int], Int, Unit] = IndexedStateT.modify(_.getOrElse(0))
    val toString: IndexedStateT[Id, Int, String, Unit] = IndexedStateT.modify(_.toString)

    val composite = for {
      _ <- listHead
      _ <- getOrElse
      _ <- toString
      r <- IndexedStateT.get[Id, String]
    } yield r

    composite.run(List(1, 2, 3)) should === (("1", "1"))
    composite.run(Nil) should === (("0", "0"))
  }

  test("traversing state is stack-safe"){
    val ns = (0 to 70000).toList
    val x = ns.traverse(_ => add1)
    x.runS(0).value should === (70001)
  }

  test("State.pure, StateT.pure and IndexedStateT.pure are consistent"){
    forAll { (s: String, i: Int) =>
      val state: State[String, Int] = State.pure(i)
      val stateT: State[String, Int] = StateT.pure(i)
      val indexedStateT: State[String, Int] = IndexedStateT.pure(i)

      state.run(s) should === (stateT.run(s))
      state.run(s) should === (indexedStateT.run(s))
    }
  }

  test("State.get, StateT.get and IndexedStateT.get are consistent") {
    forAll{ (s: String) =>
      val state: State[String, String] = State.get
      val stateT: State[String, String] = StateT.get
      val indexedStateT: State[String, String] = IndexedStateT.get

      state.run(s) should === (stateT.run(s))
      state.run(s) should === (indexedStateT.run(s))
    }
  }

  test("State.inspect, StateT.inspect and IndexedStateT.inspect are consistent") {
    forAll { (s: String, f: String => Int) =>
      val state: State[String, Int] = State.inspect(f)
      val stateT: State[String, Int] = StateT.inspect(f)
      val indexedStateT: State[String, Int] = IndexedStateT.inspect(f)

      state.run(s) should === (stateT.run(s))
      state.run(s) should === (indexedStateT.run(s))
    }
  }

  test("State.inspect, StateT.inspectF and IndexedStateT.inspectF are consistent") {
    forAll { (s: String, f: String => Int) =>
      val state: State[String, Int] = State.inspect(f)
      val stateT: State[String, Int] = StateT.inspectF(f.andThen(Eval.now))
      val indexedStateT: State[String, Int] = IndexedStateT.inspectF(f.andThen(Eval.now))

      state.run(s) should === (stateT.run(s))
      state.run(s) should === (indexedStateT.run(s))
    }
  }

  test("State.modify, StateT.modify and IndexedStateT.modify are consistent") {
    forAll { (s: String, f: String => String) =>
      val state: State[String, Unit] = State.modify(f)
      val stateT: State[String, Unit] = StateT.modify(f)
      val indexedStateT: State[String, Unit] = IndexedStateT.modify(f)

      state.run(s) should === (stateT.run(s))
      state.run(s) should === (indexedStateT.run(s))
    }
  }

  test("State.modify, StateT.modifyF and IndexedStateT.modifyF are consistent") {
    forAll { (s: String, f: String => String) =>
      val state: State[String, Unit] = State.modify(f)
      val stateT: State[String, Unit] = StateT.modifyF(f.andThen(Eval.now))
      val indexedStateT: State[String, Unit] = IndexedStateT.modifyF(f.andThen(Eval.now))

      state.run(s) should === (stateT.run(s))
      state.run(s) should === (indexedStateT.run(s))
    }
  }

  test("State.pure, StateT.lift and IndexedStateT.lift are consistent") {
    forAll { (s: String, i: Int) =>
      val state: State[String, Int] = State.pure(i)
      val stateT: State[String, Int] = StateT.lift(Eval.now(i))
      val indexedStateT: State[String, Int] = IndexedStateT.lift(Eval.now(i))

      state.run(s) should === (stateT.run(s))
      state.run(s) should === (indexedStateT.run(s))
    }
  }

  test("State.set, StateT.set and IndexedStateT.set are consistent") {
    forAll { (init: String, s: String) =>
      val state: State[String, Unit] = State.set(s)
      val stateT: StateT[Eval, String, Unit] = StateT.set(s)
      val indexedStateT: StateT[Eval, String, Unit] = IndexedStateT.set(s)

      state.run(init) should === (stateT.run(init))
      state.run(init) should === (indexedStateT.run(init))
    }
  }

  test("State.set, StateT.setF and IndexedStateT.setF are consistent") {
    forAll { (init: String, s: String) =>
      val state: State[String, Unit] = State.set(s)
      val stateT: StateT[Eval, String, Unit] = StateT.setF(Eval.now(s))
      val indexedStateT: StateT[Eval, String, Unit] = IndexedStateT.setF(Eval.now(s))

      state.run(init) should === (stateT.run(init))
      state.run(init) should === (indexedStateT.run(init))
    }
  }

  test("Cartesian syntax is usable on State") {
    val x = add1 *> add1
    x.runS(0).value should === (2)
  }

  test("Singleton and instance inspect are consistent"){
    forAll { (s: String, i: Int) =>
      State.inspect[Int, String](_.toString).run(i) should === (
        State.pure[Int, Unit](()).inspect(_.toString).run(i))
    }
  }

  test("flatMap and flatMapF consistent") {
    forAll { (stateT: StateT[Option, Long, Int], f: Int => Option[Int]) =>
      stateT.flatMap(a => StateT(s => f(a).map(b => (s, b)))) should === (stateT.flatMapF(f))
    }
  }

  test("runEmpty, runEmptyS, and runEmptyA consistent"){
    forAll { (f: StateT[List, Long, Int]) =>
      (f.runEmptyS zip f.runEmptyA) should === (f.runEmpty)
    }
  }

  test("modify identity is a noop"){
    forAll { (f: StateT[List, Long, Int]) =>
      f.modify(identity) should === (f)
    }
  }

  test("modify modifies state"){
    forAll { (f: StateT[List, Long, Int], g: Long => Long, initial: Long) =>
      f.modify(g).runS(initial) should === (f.runS(initial).map(g))
    }
  }

  test("modify doesn't affect A value"){
    forAll { (f: StateT[List, Long, Int], g: Long => Long, initial: Long) =>
      f.modify(g).runA(initial) should === (f.runA(initial))
    }
  }

  test("State.modify equivalent to get then set"){
    forAll { (f: Long => Long) =>
      val s1 = for {
        l <- State.get[Long]
        _ <- State.set(f(l))
      } yield ()

      val s2 = State.modify(f)

      s1 should === (s2)
    }
  }

  test("StateT.set equivalent to modify ignoring first param") {
    forAll { (init: String, update: String) =>
      val s1 = StateT.modify[Eval, String](_ => update)
      val s2 = StateT.set[Eval, String](update)
      s1.run(init) should === (s2.run(init))
    }
  }

  test("StateT.setF equivalent to modifyF ignoring first param") {
    forAll { (init: String, update: String) =>
      val s1 = StateT.modifyF[Eval, String](_ => Eval.now(update))
      val s2 = StateT.setF(Eval.now(update))
      s1.run(init) should === (s2.run(init))
    }
  }

  test(".get and then .run produces same state as value"){
    forAll { (s: State[Long, Int], initial: Long) =>
      val (finalS, finalA) = s.get.run(initial).value
      finalS should === (finalA)
    }
  }

  test(".get equivalent to flatMap with State.get"){
    forAll { (s: State[Long, Int]) =>
      s.get should === (s.flatMap(_ => State.get))
    }
  }

  test("StateT#transformS with identity is identity") {
    forAll { (s: StateT[List, Long, Int]) =>
      s.transformS[Long](identity, (s, i) => i) should === (s)
    }
  }

  test("StateT#transformS modifies state") {
    final case class Env(int: Int, str: String)
    val x = StateT((x: Int) => Option((x + 1, x)))
    val xx = x.transformS[Env](_.int, (e, i) => e.copy(int = i))
    val input = 5

    val got = x.run(input)
    val expected = xx.run(Env(input, "hello")).map { case (e, i) => (e.int, i) }
    got should === (expected)
  }


  implicit val iso = CartesianTests.Isomorphisms.invariant[IndexedStateT[ListWrapper, String, Int, ?]](IndexedStateT.catsDataFunctorForIndexedStateT(ListWrapper.monad))

  {
    // F has a Functor
    implicit val F: Functor[ListWrapper] = ListWrapper.functor
    // We only need a Functor on F to find a Functor on StateT
    Functor[IndexedStateT[ListWrapper, String, Int, ?]]
  }

  {
    // We only need a Functor to derive a Contravariant for IndexedStateT
    implicit val F: Functor[ListWrapper] = ListWrapper.monad
    Contravariant[IndexedStateT[ListWrapper, ?, Int, String]]
  }

  {
    // We only need a Functor to derive a Bifunctor for IndexedStateT
    implicit val F: Functor[ListWrapper] = ListWrapper.monad
    Bifunctor[IndexedStateT[ListWrapper, Int, ?, ?]]
  }

  {
    // We only need a Functor to derive a Profunctor for IndexedStateT
    implicit val F: Functor[ListWrapper] = ListWrapper.monad
    Profunctor[IndexedStateT[ListWrapper, ?, ?, String]]
  }

  {
    // F needs a Monad to do Eq on StateT
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    implicit val FS: Functor[IndexedStateT[ListWrapper, String, Int, ?]] = IndexedStateT.catsDataFunctorForIndexedStateT

    checkAll("IndexedStateT[ListWrapper, String, Int, Int]", FunctorTests[IndexedStateT[ListWrapper, String, Int, ?]].functor[Int, Int, Int])
    checkAll("Functor[IndexedStateT[ListWrapper, Int, ?]]", SerializableTests.serializable(Functor[IndexedStateT[ListWrapper, String, Int, ?]]))

    Functor[IndexedStateT[ListWrapper, String, Int, ?]]
  }

  {
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    implicit val FS: Contravariant[IndexedStateT[ListWrapper, ?, Int, Int]] = IndexedStateT.catsDataContravariantForIndexedStateT

    checkAll("IndexedStateT[ListWrapper, Int, Int, Int]", ContravariantTests[IndexedStateT[ListWrapper, ?, Int, Int]].contravariant[Int, Int, Int])
    checkAll("Contravariant[IndexedStateT[ListWrapper, ?, Int, Int]]", SerializableTests.serializable(Contravariant[IndexedStateT[ListWrapper, ?, Int, Int]]))

    Contravariant[IndexedStateT[ListWrapper, ?, Int, Int]]
  }

  {
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    implicit val FS: Bifunctor[IndexedStateT[ListWrapper, Int, ?, ?]] = IndexedStateT.catsDataBifunctorForIndexedStateT

    checkAll("IndexedStateT[ListWrapper, Int, String, Int]", BifunctorTests[IndexedStateT[ListWrapper, Int, ?, ?]].bifunctor[String, String, String, Int, Int, Int])
    checkAll("Bifunctor[IndexedStateT[ListWrapper, Int, ?, ?]]", SerializableTests.serializable(Bifunctor[IndexedStateT[ListWrapper, Int, ?, ?]]))

    Bifunctor[IndexedStateT[ListWrapper, Int, ?, ?]]
  }

  {
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    implicit val FS: Profunctor[IndexedStateT[ListWrapper, ?, ?, Int]] = IndexedStateT.catsDataProfunctorForIndexedStateT

    checkAll("IndexedStateT[ListWrapper, String, Int, Int]", ProfunctorTests[IndexedStateT[ListWrapper, ?, ?, Int]].profunctor[String, String, String, Int, Int, Int])
    checkAll("Profunctor[IndexedStateT[ListWrapper, ?, ?, Int]]", SerializableTests.serializable(Profunctor[IndexedStateT[ListWrapper, ?, ?, Int]]))

    Profunctor[IndexedStateT[ListWrapper, ?, ?, Int]]
  }

  {
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    implicit val FS: Strong[IndexedStateT[ListWrapper, ?, ?, Int]] = IndexedStateT.catsDataStrongForIndexedStateT

    checkAll("IndexedStateT[ListWrapper, String, Int, Int]", StrongTests[IndexedStateT[ListWrapper, ?, ?, Int]].strong[String, String, String, Int, Int, Int])
    checkAll("Strong[IndexedStateT[ListWrapper, ?, ?, Int]]", SerializableTests.serializable(Strong[IndexedStateT[ListWrapper, ?, ?, Int]]))

    Strong[IndexedStateT[ListWrapper, ?, ?, Int]]
  }

  {
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    implicit val FS: Arrow[IndexedStateT[ListWrapper, ?, ?, Int]] = IndexedStateT.catsDataArrowForIndexedStateT

    checkAll("IndexedStateT[ListWrapper, String, Int, Int]", ArrowTests[IndexedStateT[ListWrapper, ?, ?, Int]].arrow[String, String, String, Int, Int, Int])
    checkAll("Arrow[IndexedStateT[ListWrapper, ?, ?, Int]]", SerializableTests.serializable(Arrow[IndexedStateT[ListWrapper, ?, ?, Int]]))

    Arrow[IndexedStateT[ListWrapper, ?, ?, Int]]
  }

  {
    // F has a Monad
    implicit val F = ListWrapper.monad

    checkAll("IndexedStateT[ListWrapper, Int, Int]", MonadTests[IndexedStateT[ListWrapper, Int, Int, ?]].monad[Int, Int, Int])
    checkAll("Monad[StateT[ListWrapper, Int, ?]]", SerializableTests.serializable(Monad[IndexedStateT[ListWrapper, Int, Int, ?]]))

    Monad[IndexedStateT[ListWrapper, Int, Int, ?]]
    FlatMap[IndexedStateT[ListWrapper, Int, Int, ?]]
    Applicative[IndexedStateT[ListWrapper, Int, Int, ?]]
    Apply[IndexedStateT[ListWrapper, Int, Int, ?]]
    Functor[IndexedStateT[ListWrapper, Int, Int, ?]]
  }

  {
    // F has a Monad and a SemigroupK
    implicit val F = ListWrapper.monad
    implicit val S = ListWrapper.semigroupK

    checkAll("IndexedStateT[ListWrapper, Int, Int]", SemigroupKTests[IndexedStateT[ListWrapper, Int, Int, ?]].semigroupK[Int])
    checkAll("SemigroupK[IndexedStateT[ListWrapper, Int, ?]]", SerializableTests.serializable(SemigroupK[IndexedStateT[ListWrapper, String, Int, ?]]))
  }

  {
    // F has an Alternative
    implicit val G = ListWrapper.monad
    implicit val F = ListWrapper.alternative
    val SA = IndexedStateT.catsDataAlternativeForIndexedStateT[ListWrapper, Int](ListWrapper.monad, ListWrapper.alternative)

    checkAll("IndexedStateT[ListWrapper, Int, Int, Int]", AlternativeTests[IndexedStateT[ListWrapper, Int, Int, ?]](SA).monoidK[Int])
    checkAll("Alternative[IndexedStateT[ListWrapper, Int, Int, ?]]", SerializableTests.serializable(SA))

    Monad[IndexedStateT[ListWrapper, Int, Int, ?]]
    FlatMap[IndexedStateT[ListWrapper, Int, Int, ?]]
    Alternative[IndexedStateT[ListWrapper, Int, Int, ?]]
    Applicative[IndexedStateT[ListWrapper, Int, Int, ?]]
    Apply[IndexedStateT[ListWrapper, Int, Int, ?]]
    Functor[IndexedStateT[ListWrapper, Int, Int, ?]]
    MonoidK[IndexedStateT[ListWrapper, Int, Int, ?]]
    SemigroupK[IndexedStateT[ListWrapper, Int, Int, ?]]
  }

  {
    implicit val iso = CartesianTests.Isomorphisms.invariant[State[Long, ?]]

    checkAll("State[Long, ?]", MonadTests[State[Long, ?]].monad[Int, Int, Int])
    checkAll("Monad[State[Long, ?]]", SerializableTests.serializable(Monad[State[Long, ?]]))
  }

  {
    // F has a MonadError
    implicit val iso = CartesianTests.Isomorphisms.invariant[StateT[Option, Int, ?]]
    implicit val eqEitherTFA: Eq[EitherT[StateT[Option, Int , ?], Unit, Int]] = EitherT.catsDataEqForEitherT[StateT[Option, Int , ?], Unit, Int]

    checkAll("StateT[Option, Int, Int]", MonadErrorTests[StateT[Option, Int, ?], Unit].monadError[Int, Int, Int])
    checkAll("MonadError[StateT[Option, Int, ?], Unit]", SerializableTests.serializable(MonadError[StateT[Option, Int , ?], Unit]))
  }

}

object IndexedStateTTests extends IndexedStateTTestsInstances {
  implicit def stateEq[S:Eq:Arbitrary, A:Eq]: Eq[State[S, A]] =
    indexedStateTEq[Eval, S, S, A]

  val add1: State[Int, Int] = State(n => (n + 1, n))
}

sealed trait IndexedStateTTestsInstances {

  implicit def indexedStateTEq[F[_], SA, SB, A](implicit SA: Arbitrary[SA], FSB: Eq[F[(SB, A)]], F: FlatMap[F]): Eq[IndexedStateT[F, SA, SB, A]] =
    Eq.by[IndexedStateT[F, SA, SB, A], SA => F[(SB, A)]](state =>
      s => state.run(s))
}
