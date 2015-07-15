package cats
package state

import cats.tests.CatsSuite
import cats.laws.discipline.{ArbitraryK, MonadStateTests, MonoidKTests, SerializableTests}
import cats.laws.discipline.eq._
import org.scalacheck.{Arbitrary, Gen, Prop}, Prop.forAll

class StateTTests extends CatsSuite {
  import StateTTests._

  test("basic state usage"){
    assert(add1.run(1).run == (2 -> 1))
  }

  test("traversing state is stack-safe"){
    val ns = (0 to 100000).toList
    val x = ns.traverseU(_ => add1)
    assert(x.runS(0).run == 100001)
  }

  test("State.pure and StateT.pure are consistent")(check {
    forAll { (s: String, i: Int) =>
      val state: State[String, Int] = State.pure(i)
      val stateT: State[String, Int] = StateT.pure(i)
      state.run(s).run == stateT.run(s).run
    }
  })

  test("Apply syntax is usable on State") {
    val x = add1 *> add1
    assert(x.runS(0).run == 2)
  }

  test("Singleton and instance inspect are consistent")(check {
    forAll { (s: String, i: Int) =>
      State.inspect[Int, String](_.toString).run(i).run ==
        State.pure[Int, Unit](()).inspect(_.toString).run(i).run
    }
  })

  checkAll("StateT[Option, Int, Int]", MonadStateTests[StateT[Option, ?, ?], Int].monadState[Int, Int, Int])
  checkAll("MonadState[StateT[Option, ?, ?], Int]", SerializableTests.serializable(MonadState[StateT[Option, ?, ?], Int]))
}

object StateTTests {

  // This seems unnecessarily complicated. I think having our laws require
  // ArbitraryK is overly constraining.
  // It seems like I should just be able to use an Arbitrary[StateT[F, S, A]]
  // that is derived from an Arbitrary[F[S => F[(S, A)]]]
  implicit def stateArbitrary[F[_], S, A](implicit F: ArbitraryK[F], S: Arbitrary[S], A: Arbitrary[A]): Arbitrary[StateT[F, S, A]] =
    Arbitrary(for {
      sa <- F.synthesize[(S, A)].arbitrary
      f <- F.synthesize[S => F[(S, A)]](Arbitrary(Gen.const(_ => sa))).arbitrary
    } yield StateT.applyF(f))

  implicit def stateArbitraryK[F[_], S](implicit F: ArbitraryK[F], S: Arbitrary[S]): ArbitraryK[StateT[F, S, ?]] =
    new ArbitraryK[StateT[F, S, ?]]{ def synthesize[A: Arbitrary]: Arbitrary[StateT[F, S, A]] = stateArbitrary[F, S, A] }

  implicit def stateEq[F[_], S, A](implicit S: Arbitrary[S], FSA: Eq[F[(S, A)]], F: FlatMap[F]): Eq[StateT[F, S, A]] =
    Eq.by[StateT[F, S, A], S => F[(S, A)]](state =>
      s => state.run(s))

  val add1: State[Int, Int] = State(n => (n + 1, n))
}
