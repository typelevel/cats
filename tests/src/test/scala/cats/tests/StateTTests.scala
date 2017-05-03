package cats
package tests

import cats.data.{State, StateT, EitherT}
import cats.kernel.instances.tuple._
import cats.laws.discipline._
import cats.laws.discipline.eq._
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary

class StateTTests extends CatsSuite {
  import StateTTests._

  test("basic state usage"){
    add1.run(1).value should === (2 -> 1)
  }

  test("traversing state is stack-safe"){
    val ns = (0 to 100000).toList
    val x = ns.traverse(_ => add1)
    x.runS(0).value should === (100001)
  }

  test("State.pure and StateT.pure are consistent"){
    forAll { (s: String, i: Int) =>
      val state: State[String, Int] = State.pure(i)
      val stateT: State[String, Int] = StateT.pure(i)
      state.run(s) should === (stateT.run(s))
    }
  }

  test("State.get and StateT.get are consistent") {
    forAll{ (s: String) =>
      val state: State[String, String] = State.get
      val stateT: State[String, String] = StateT.get
      state.run(s) should === (stateT.run(s))
    }
  }

  test("State.inspect and StateT.inspect are consistent") {
    forAll { (s: String, f: String => Int) =>
      val state: State[String, Int] = State.inspect(f)
      val stateT: State[String, Int] = StateT.inspect(f)
      state.run(s) should === (stateT.run(s))
    }
  }

  test("State.inspect and StateT.inspectF are consistent") {
    forAll { (s: String, f: String => Int) =>
      val state: State[String, Int] = State.inspect(f)
      val stateT: State[String, Int] = StateT.inspectF(f.andThen(Eval.now))
      state.run(s) should === (stateT.run(s))
    }
  }

  test("State.modify and StateT.modify are consistent") {
    forAll { (s: String, f: String => String) =>
      val state: State[String, Unit] = State.modify(f)
      val stateT: State[String, Unit] = StateT.modify(f)
      state.run(s) should === (stateT.run(s))
    }
  }

  test("State.modify and StateT.modifyF are consistent") {
    forAll { (s: String, f: String => String) =>
      val state: State[String, Unit] = State.modify(f)
      val stateT: State[String, Unit] = StateT.modifyF(f.andThen(Eval.now))
      state.run(s) should === (stateT.run(s))
    }
  }

  test("State.pure and StateT.lift are consistent") {
    forAll { (s: String, i: Int) =>
      val state: State[String, Int] = State.pure(i)
      val stateT: State[String, Int] = StateT.lift(Eval.now(i))
      state.run(s) should === (stateT.run(s))
    }
  }

  test("State.set and StateT.set are consistent") {
    forAll { (init: String, s: String) =>
      val state: State[String, Unit] = State.set(s)
      val stateT: StateT[Eval, String, Unit] = StateT.set(s)
      state.run(init) should === (stateT.run(init))
    }
  }

  test("State.set and StateT.setF are consistent") {
    forAll { (init: String, s: String) =>
      val state: State[String, Unit] = State.set(s)
      val stateT: StateT[Eval, String, Unit] = StateT.setF(Eval.now(s))
      state.run(init) should === (stateT.run(init))
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


  implicit val iso = CartesianTests.Isomorphisms.invariant[StateT[ListWrapper, Int, ?]](StateT.catsDataFunctorForStateT(ListWrapper.monad))

  {
    // F has a Functor
    implicit val F: Functor[ListWrapper] = ListWrapper.monad
    // We only need a Functor on F to find a Functor on StateT
    Functor[StateT[ListWrapper, Int, ?]]
  }

  {
    // F needs a Monad to do Eq on StateT
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    implicit val FS: Functor[StateT[ListWrapper, Int, ?]] = StateT.catsDataFunctorForStateT

    checkAll("StateT[ListWrapper, Int, Int]", FunctorTests[StateT[ListWrapper, Int, ?]].functor[Int, Int, Int])
    checkAll("Functor[StateT[ListWrapper, Int, ?]]", SerializableTests.serializable(Functor[StateT[ListWrapper, Int, ?]]))

    Functor[StateT[ListWrapper, Int, ?]]
  }

  {
    // F has a Monad
    implicit val F = ListWrapper.monad

    checkAll("StateT[ListWrapper, Int, Int]", MonadStateTests[StateT[ListWrapper, Int, ?], Int].monadState[Int, Int, Int])
    checkAll("MonadState[StateT[ListWrapper, Int, ?], Int]", SerializableTests.serializable(MonadState[StateT[ListWrapper, Int, ?], Int]))

    Monad[StateT[ListWrapper, Int, ?]]
    FlatMap[StateT[ListWrapper, Int, ?]]
    Applicative[StateT[ListWrapper, Int, ?]]
    Apply[StateT[ListWrapper, Int, ?]]
    Functor[StateT[ListWrapper, Int, ?]]
  }

  {
    // F has a Monad
    implicit val F = ListWrapper.monad

    checkAll("StateT[ListWrapper, Int, Int]", MonadTests[StateT[ListWrapper, Int, ?]].monad[Int, Int, Int])
    checkAll("Monad[StateT[ListWrapper, Int, ?]]", SerializableTests.serializable(Monad[StateT[ListWrapper, Int, ?]]))

    Monad[StateT[ListWrapper, Int, ?]]
    FlatMap[StateT[ListWrapper, Int, ?]]
    Applicative[StateT[ListWrapper, Int, ?]]
    Apply[StateT[ListWrapper, Int, ?]]
    Functor[StateT[ListWrapper, Int, ?]]
  }

  {
    // F has a Monad and a SemigroupK
    implicit def F = ListWrapper.monad
    implicit def S = ListWrapper.semigroupK

    checkAll("StateT[ListWrapper, Int, Int]", SemigroupKTests[StateT[ListWrapper, Int, ?]].semigroupK[Int])
    checkAll("SemigroupK[StateT[ListWrapper, Int, ?]]", SerializableTests.serializable(SemigroupK[StateT[ListWrapper, Int, ?]]))
  }

  {
    // F has a MonadCombine
    implicit def F = ListWrapper.monadCombine

    checkAll("StateT[ListWrapper, Int, Int]", MonadCombineTests[StateT[ListWrapper, Int, ?]].monadCombine[Int, Int, Int])
    checkAll("MonadCombine[StateT[ListWrapper, Int, ?]]", SerializableTests.serializable(MonadCombine[StateT[ListWrapper, Int, ?]]))

    Monad[StateT[ListWrapper, Int, ?]]
    FlatMap[StateT[ListWrapper, Int, ?]]
    Alternative[StateT[ListWrapper, Int, ?]]
    Applicative[StateT[ListWrapper, Int, ?]]
    Apply[StateT[ListWrapper, Int, ?]]
    Functor[StateT[ListWrapper, Int, ?]]
    MonoidK[StateT[ListWrapper, Int, ?]]
    SemigroupK[StateT[ListWrapper, Int, ?]]
  }

  {
    implicit val iso = CartesianTests.Isomorphisms.invariant[State[Long, ?]]

    checkAll("State[Long, ?]", MonadStateTests[State[Long, ?], Long].monadState[Int, Int, Int])
    checkAll("MonadState[State[Long, ?], Long]", SerializableTests.serializable(MonadState[State[Long, ?], Long]))

    checkAll("State[Long, ?]", MonadTests[State[Long, ?]].monad[Int, Int, Int])
    checkAll("Monad[State[Long, ?]]", SerializableTests.serializable(Monad[State[Long, ?]]))
  }

  {
    // F has a MonadError
    implicit val iso = CartesianTests.Isomorphisms.invariant[StateT[Option, Int, ?]]
    implicit val eqEitherTFA: Eq[EitherT[StateT[Option, Int , ?], Unit, Int]] = EitherT.catsDataEqForEitherT[StateT[Option, Int , ?], Unit, Int]

    checkAll("StateT[Option, Int, Int]", MonadErrorTests[StateT[Option, Int , ?], Unit].monadError[Int, Int, Int])
    checkAll("MonadError[StateT[Option, Int , ?], Unit]", SerializableTests.serializable(MonadError[StateT[Option, Int , ?], Unit]))
  }

}

object StateTTests extends StateTTestsInstances {
  implicit def stateEq[S:Eq:Arbitrary, A:Eq]: Eq[State[S, A]] =
    stateTEq[Eval, S, A]

  val add1: State[Int, Int] = State(n => (n + 1, n))
}

sealed trait StateTTestsInstances {

  implicit def stateTEq[F[_], S, A](implicit S: Arbitrary[S], FSA: Eq[F[(S, A)]], F: FlatMap[F]): Eq[StateT[F, S, A]] =
    Eq.by[StateT[F, S, A], S => F[(S, A)]](state =>
      s => state.run(s))
}
