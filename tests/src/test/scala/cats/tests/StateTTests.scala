package cats
package tests

import cats.laws.discipline.{MonoidalTests, MonadStateTests, MonoidKTests, SerializableTests}
import cats.state.{State, StateT}
import cats.laws.discipline.eq._
import cats.laws.discipline.arbitrary._
import org.scalacheck.{Arbitrary, Gen}

class StateTTests extends CatsSuite {
  import StateTTests._

  test("basic state usage"){
    add1.run(1).value should === (2 -> 1)
  }

  test("traversing state is stack-safe"){
    val ns = (0 to 100000).toList
    val x = ns.traverseU(_ => add1)
    x.runS(0).value should === (100001)
  }

  test("State.pure and StateT.pure are consistent"){
    forAll { (s: String, i: Int) =>
      val state: State[String, Int] = State.pure(i)
      val stateT: State[String, Int] = StateT.pure(i)
      state.run(s) should === (stateT.run(s))
    }
  }

  test("Monoidal syntax is usable on State") {
    val x = add1 *> add1
    x.runS(0).value should === (2)
  }

  test("Singleton and instance inspect are consistent"){
    forAll { (s: String, i: Int) =>
      State.inspect[Int, String](_.toString).run(i) should === (
        State.pure[Int, Unit](()).inspect(_.toString).run(i))
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

  {
    implicit val iso = MonoidalTests.Isomorphisms.invariant[StateT[Option, Int, ?]]
    checkAll("StateT[Option, Int, Int]", MonadStateTests[StateT[Option, Int, ?], Int].monadState[Int, Int, Int])
    checkAll("MonadState[StateT[Option, ?, ?], Int]", SerializableTests.serializable(MonadState[StateT[Option, Int, ?], Int]))
  }

  {
    implicit val iso = MonoidalTests.Isomorphisms.invariant[State[Long, ?]]
    checkAll("State[Long, ?]", MonadStateTests[State[Long, ?], Long].monadState[Int, Int, Int])
    checkAll("MonadState[State[Long, ?], Long]", SerializableTests.serializable(MonadState[State[Long, ?], Long]))
  }
}

object StateTTests extends StateTTestsInstances {
  implicit def stateEq[S:Eq:Arbitrary, A:Eq]: Eq[State[S, A]] =
    stateTEq[Eval, S, A]

  implicit def stateArbitrary[S: Arbitrary, A: Arbitrary]: Arbitrary[State[S, A]] =
    stateTArbitrary[Eval, S, A]

  val add1: State[Int, Int] = State(n => (n + 1, n))
}

sealed trait StateTTestsInstances {
  implicit def stateTArbitrary[F[_]: Applicative, S, A](implicit F: Arbitrary[S => F[(S, A)]]): Arbitrary[StateT[F, S, A]] =
    Arbitrary(F.arbitrary.map(f => StateT(f)))

  implicit def stateTEq[F[_], S, A](implicit S: Arbitrary[S], FSA: Eq[F[(S, A)]], F: FlatMap[F]): Eq[StateT[F, S, A]] =
    Eq.by[StateT[F, S, A], S => F[(S, A)]](state =>
      s => state.run(s))
}
