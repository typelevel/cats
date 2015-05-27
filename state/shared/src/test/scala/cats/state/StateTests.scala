package cats
package state

import cats.tests.CatsSuite
import cats.laws.discipline.{ArbitraryK, MonadTests, MonoidKTests, SerializableTests}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import org.scalacheck.{Arbitrary, Gen}

class StateTests extends CatsSuite {
  import StateTests._

  test("basic state usage"){
    assert(add1.run(1).run == (2 -> 1))
  }

  test("traversing state is stack-safe"){
    val ns = (0 to 100000).toList
    // syntax doesn't work here. Should look into why
    val x = Traverse[List].traverse[State[Int, ?], Int, Int](ns)(_ => add1)
    assert(x.runS(0).run == 100001)
  }

  checkAll("StateT[Option, Int, Int]", MonadTests[StateT[Option, Int, ?]].monad[Int, Int, Int])
  checkAll("Monad[StateT[Option, Int, ?]]", SerializableTests.serializable(Monad[StateT[Option, Int, ?]]))
}

object StateTests {

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
