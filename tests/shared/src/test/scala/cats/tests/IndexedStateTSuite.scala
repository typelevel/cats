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
import cats.arrow.Profunctor
import cats.arrow.Strong
import cats.data.EitherT
import cats.data.IndexedStateT
import cats.data.State
import cats.data.StateT
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import cats.platform.Platform
import cats.syntax.all.*
import org.scalacheck.Prop.*
import org.scalacheck.Test.Parameters

class IndexedStateTSuite extends CatsSuite {

  implicit override val scalaCheckTestParameters: Parameters =
    checkConfiguration.withMaxSize(checkConfiguration.minSize + 5)

  import IndexedStateTSuite.*

  test("basic state usage") {
    assert(add1.run(1).value === (2 -> 1))
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

    assert(composite.run(List(1, 2, 3)) === (("1", "1")))
    assert(composite.run(Nil) === (("0", "0")))
  }

  test("traversing state is stack-safe") {
    val ns = (0 to 70000).toList
    val x = ns.traverse(_ => add1)
    assert(x.runS(0).value === 70001)
  }

  test("State.pure, StateT.pure and IndexedStateT.pure are consistent") {
    forAll { (s: String, i: Int) =>
      val state: State[String, Int] = State.pure(i)
      val stateT: State[String, Int] = StateT.pure(i)
      val indexedStateT: State[String, Int] = IndexedStateT.pure(i)

      assert(state.run(s) === (stateT.run(s)))
      assert(state.run(s) === (indexedStateT.run(s)))
    }
  }

  test("State.empty, StateT.empty and IndexedStateT.empty are consistent") {
    forAll { (s: String) =>
      val state: State[String, Int] = State.empty
      val stateT: State[String, Int] = StateT.empty
      val indexedStateT: State[String, Int] = IndexedStateT.empty

      assert(state.run(s) === (stateT.run(s)))
      assert(state.run(s) === (indexedStateT.run(s)))
    }
  }

  test("State.get, StateT.get and IndexedStateT.get are consistent") {
    forAll { (s: String) =>
      val state: State[String, String] = State.get
      val stateT: State[String, String] = StateT.get
      val indexedStateT: State[String, String] = IndexedStateT.get

      assert(state.run(s) === (stateT.run(s)))
      assert(state.run(s) === (indexedStateT.run(s)))
    }
  }

  test("State.inspect, StateT.inspect and IndexedStateT.inspect are consistent") {
    forAll { (s: String, f: String => Int) =>
      val state: State[String, Int] = State.inspect(f)
      val stateT: State[String, Int] = StateT.inspect(f)
      val indexedStateT: State[String, Int] = IndexedStateT.inspect(f)

      assert(state.run(s) === (stateT.run(s)))
      assert(state.run(s) === (indexedStateT.run(s)))
    }
  }

  test("State.inspect, StateT.inspectF and IndexedStateT.inspectF are consistent") {
    forAll { (s: String, f: String => Int) =>
      val state: State[String, Int] = State.inspect(f)
      val stateT: State[String, Int] = StateT.inspectF(f.andThen(Eval.now))
      val indexedStateT: State[String, Int] = IndexedStateT.inspectF(f.andThen(Eval.now))

      assert(state.run(s) === (stateT.run(s)))
      assert(state.run(s) === (indexedStateT.run(s)))
    }
  }

  test("State.modify, StateT.modify and IndexedStateT.modify are consistent") {
    forAll { (s: String, f: String => String) =>
      val state: State[String, Unit] = State.modify(f)
      val stateT: State[String, Unit] = StateT.modify(f)
      val indexedStateT: State[String, Unit] = IndexedStateT.modify(f)

      assert(state.run(s) === (stateT.run(s)))
      assert(state.run(s) === (indexedStateT.run(s)))
    }
  }

  test("State.modify, StateT.modifyF and IndexedStateT.modifyF are consistent") {
    forAll { (s: String, f: String => String) =>
      val state: State[String, Unit] = State.modify(f)
      val stateT: State[String, Unit] = StateT.modifyF(f.andThen(Eval.now))
      val indexedStateT: State[String, Unit] = IndexedStateT.modifyF(f.andThen(Eval.now))

      assert(state.run(s) === (stateT.run(s)))
      assert(state.run(s) === (indexedStateT.run(s)))
    }
  }

  test("State.pure, StateT.liftF and IndexedStateT.liftF are consistent") {
    forAll { (s: String, i: Int) =>
      val state: State[String, Int] = State.pure(i)
      val stateT: State[String, Int] = StateT.liftF(Eval.now(i))
      val indexedStateT: State[String, Int] = IndexedStateT.liftF(Eval.now(i))

      assert(state.run(s) === (stateT.run(s)))
      assert(state.run(s) === (indexedStateT.run(s)))
    }
  }

  test("State.set, StateT.set and IndexedStateT.set are consistent") {
    forAll { (init: String, s: String) =>
      val state: State[String, Unit] = State.set(s)
      val stateT: StateT[Eval, String, Unit] = StateT.set(s)
      val indexedStateT: StateT[Eval, String, Unit] = IndexedStateT.set(s)

      assert(state.run(init) === (stateT.run(init)))
      assert(state.run(init) === (indexedStateT.run(init)))
    }
  }

  test("State.set, StateT.setF and IndexedStateT.setF are consistent") {
    forAll { (init: String, s: String) =>
      val state: State[String, Unit] = State.set(s)
      val stateT: StateT[Eval, String, Unit] = StateT.setF(Eval.now(s))
      val indexedStateT: StateT[Eval, String, Unit] = IndexedStateT.setF(Eval.now(s))

      assert(state.run(init) === (stateT.run(init)))
      assert(state.run(init) === (indexedStateT.run(init)))
    }
  }

  test("Semigroupal syntax is usable on State") {
    val x = add1 *> add1
    assert(x.runS(0).value === 2)
  }

  test("Singleton and instance inspect are consistent") {
    forAll { (s: String, i: Int) =>
      assert(State.inspect[Int, String](_.toString).run(i) === (State.pure[Int, Unit](()).inspect(_.toString).run(i)))
    }
  }

  test("flatMap and flatMapF consistent") {
    forAll { (stateT: StateT[Option, MiniInt, Int], f: Int => Option[Int]) =>
      assert(stateT.flatMap(a => StateT(s => f(a).map(b => (s, b)))) === (stateT.flatMapF(f)))
    }
  }

  test("runEmpty, runEmptyS, and runEmptyA consistent") {
    forAll { (f: StateT[List, Long, Int]) =>
      assert((f.runEmptyS.zip(f.runEmptyA)) === (f.runEmpty))
    }
  }

  test("modify identity is a noop") {
    forAll { (f: StateT[List, MiniInt, Int]) =>
      assert(f.modify(identity) === f)
    }
  }

  test("modify modifies state") {
    forAll { (f: StateT[List, Long, Int], g: Long => Long, initial: Long) =>
      assert(f.modify(g).runS(initial) === (f.runS(initial).map(g)))
    }
  }

  test("modify doesn't affect A value") {
    forAll { (f: StateT[List, Long, Int], g: Long => Long, initial: Long) =>
      assert(f.modify(g).runA(initial) === (f.runA(initial)))
    }
  }

  test("State.modify equivalent to get then set") {
    forAll { (f: MiniInt => MiniInt) =>
      val s1 = for {
        l <- State.get[MiniInt]
        _ <- State.set(f(l))
      } yield ()

      val s2 = State.modify(f)

      assert(s1 === s2)
    }
  }

  test("StateT.set equivalent to modify ignoring first param") {
    forAll { (init: String, update: String) =>
      val s1 = StateT.modify[Eval, String](_ => update)
      val s2 = StateT.set[Eval, String](update)
      assert(s1.run(init) === (s2.run(init)))
    }
  }

  test("StateT.setF equivalent to modifyF ignoring first param") {
    forAll { (init: String, update: String) =>
      val s1 = StateT.modifyF[Eval, String](_ => Eval.now(update))
      val s2 = StateT.setF(Eval.now(update))
      assert(s1.run(init) === (s2.run(init)))
    }
  }

  test(".get and then .run produces same state as value") {
    forAll { (s: State[Long, Int], initial: Long) =>
      val (finalS, finalA) = s.get.run(initial).value
      assert(finalS === finalA)
    }
  }

  test(".get equivalent to flatMap with State.get") {
    forAll { (s: State[MiniInt, Int]) =>
      assert(s.get === (s.flatMap(_ => State.get)))
    }
  }

  test("StateT#transformS with identity is identity") {
    forAll { (s: StateT[List, MiniInt, Int]) =>
      assert(s.transformS[MiniInt](identity, (s, i) => i) === s)
    }
  }

  test("StateT#mapK transforms effect") {
    val f: Eval ~> Id = new (Eval ~> Id) { def apply[A](a: Eval[A]): A = a.value }
    forAll { (state: StateT[Eval, Long, Int], initial: Long) =>
      assert(state.mapK(f).runA(initial) === (state.runA(initial).value))
    }
  }

  test("StateT#transformS modifies state") {
    final case class Env(int: Int, str: String)
    val x = StateT((x: Int) => Option((x + 1, x)))
    val xx = x.transformS[Env](_.int, (e, i) => e.copy(int = i))
    val input = 5

    val got = x.run(input)
    val expected = xx.run(Env(input, "hello")).map { case (e, i) => (e.int, i) }
    assert(got === expected)
  }

  private val stackSafeTestSize =
    if (Platform.isJvm) 100000 else 100

  test("repeated map is stack safe") {
    val unit = StateT.pure[Eval, Unit, Int](0)
    val count = stackSafeTestSize
    val result = (0 until count).foldLeft(unit) { (acc, _) =>
      acc.map(_ + 1)
    }
    assert(result.run(()).value === (((), count)))
  }

  test("flatMap is stack safe on repeated left binds when F is") {
    val unit = StateT.pure[Eval, Unit, Unit](())
    val count = stackSafeTestSize
    val result = (0 until count).foldLeft(unit) { (acc, _) =>
      acc.flatMap(_ => unit)
    }
    assert(result.run(()).value === (((), ())))
  }

  test("flatMap is stack safe on repeated right binds when F is") {
    val unit = StateT.pure[Eval, Unit, Unit](())
    val count = stackSafeTestSize
    val result = (0 until count).foldLeft(unit) { (acc, _) =>
      unit.flatMap(_ => acc)
    }
    assert(result.run(()).value === (((), ())))
  }

  test("untilDefinedM works") {
    val counter = State { (i: Int) =>
      val res = if (i > stackSafeTestSize) Some(i) else None
      (i + 1, res)
    }

    assert(counter.untilDefinedM.run(0).value === ((stackSafeTestSize + 2, stackSafeTestSize + 1)))
  }

  test("foreverM works") {
    val step = StateT[Either[Int, *], Int, Unit] { i =>
      if (i > stackSafeTestSize) Left(i) else Right((i + 1, ()))
    }
    step.foreverM.run(0) match {
      case Left(big)     => assert(big === stackSafeTestSize + 1)
      case Right((_, _)) => fail("unreachable code due to Nothing, but scalac won't let us match on it")
    }
  }

  test("iterateForeverM works") {
    val result = 0.iterateForeverM { i =>
      StateT[Either[Int, *], Int, Int] { j =>
        if (j > stackSafeTestSize) Left(j) else Right((j + 1, i + 1))
      }
    }
    result.run(0) match {
      case Left(sum)     => assert(sum === stackSafeTestSize + 1)
      case Right((_, _)) => fail("unreachable code due to Nothing, but scalac won't let us match on it")
    }
  }

  test("fromState correctly turns State[A, F[B]] into StateT[F, A, B]") {
    val state: State[Int, Option[Int]] = add1.map(Some.apply)
    forAll { (initial: Int) =>
      assert(StateT.fromState(state).run(initial).get === {
        val (s, Some(result)) = state.run(initial).value: @unchecked // non-exhaustive match warning
        (s, result)
      })
    }
  }

  implicit val iso: Isomorphisms[IndexedStateT[ListWrapper, String, Int, *]] =
    Isomorphisms.invariant[IndexedStateT[ListWrapper, String, Int, *]](
      IndexedStateT.catsDataFunctorForIndexedStateT(ListWrapper.monad)
    )

  {
    // F has a Functor
    implicit val F: Functor[ListWrapper] = ListWrapper.functor
    // We only need a Functor on F to find a Functor on StateT
    Functor[IndexedStateT[ListWrapper, String, Int, *]]
  }

  {
    // We only need a Functor to derive a Contravariant for IndexedStateT
    implicit val F: Functor[ListWrapper] = ListWrapper.monad
    Contravariant[IndexedStateT[ListWrapper, *, Int, String]]
  }

  {
    // We only need a Functor to derive a Bifunctor for IndexedStateT
    implicit val F: Functor[ListWrapper] = ListWrapper.monad
    Bifunctor[IndexedStateT[ListWrapper, Int, *, *]]
  }

  {
    // We only need a Functor to derive a Profunctor for IndexedStateT
    implicit val F: Functor[ListWrapper] = ListWrapper.monad
    Profunctor[IndexedStateT[ListWrapper, *, *, String]]
  }

  checkAll("IndexedStateT[Eval, MiniInt, String, *]", DeferTests[IndexedStateT[Eval, MiniInt, String, *]].defer[Int])

  {
    // F needs a Monad to do Eq on StateT
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    implicit val FS: Functor[IndexedStateT[ListWrapper, String, Int, *]] = IndexedStateT.catsDataFunctorForIndexedStateT

    checkAll("IndexedStateT[ListWrapper, MiniInt, Int, Int]",
             FunctorTests[IndexedStateT[ListWrapper, MiniInt, Int, *]].functor[Int, Int, Int]
    )
    checkAll("Functor[IndexedStateT[ListWrapper, Int, *]]",
             SerializableTests.serializable(Functor[IndexedStateT[ListWrapper, String, Int, *]])
    )

    Functor[IndexedStateT[ListWrapper, String, Int, *]]
  }

  {
    implicit val F0: Monad[ListWrapper] = ListWrapper.monad
    implicit val FF: FunctorFilter[ListWrapper] = ListWrapper.functorFilter

    checkAll("IndexedStateT[ListWrapper, MiniInt, Int, *]",
             FunctorFilterTests[IndexedStateT[ListWrapper, MiniInt, Int, *]].functorFilter[Int, Int, Int]
    )
    checkAll(
      "FunctorFilter[IndexedStateT[ListWrapper, MiniInt, Int, *]]",
      SerializableTests.serializable(FunctorFilter[IndexedStateT[ListWrapper, MiniInt, Int, *]])
    )

    FunctorFilter[IndexedStateT[ListWrapper, String, Int, *]]
  }

  {
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    implicit val FS: Contravariant[IndexedStateT[ListWrapper, *, Int, Int]] =
      IndexedStateT.catsDataContravariantForIndexedStateT

    checkAll("IndexedStateT[ListWrapper, *, Int, Boolean]",
             ContravariantTests[IndexedStateT[ListWrapper, *, Int, Int]].contravariant[MiniInt, Int, Boolean]
    )
    checkAll("Contravariant[IndexedStateT[ListWrapper, *, Int, Int]]",
             SerializableTests.serializable(Contravariant[IndexedStateT[ListWrapper, *, Int, Int]])
    )

    Contravariant[IndexedStateT[ListWrapper, *, Int, Int]]
  }

  {
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    implicit val FS: Bifunctor[IndexedStateT[ListWrapper, Int, *, *]] = IndexedStateT.catsDataBifunctorForIndexedStateT

    checkAll(
      "IndexedStateT[ListWrapper, MiniInt, String, Int]",
      BifunctorTests[IndexedStateT[ListWrapper, MiniInt, *, *]].bifunctor[String, String, String, Int, Int, Int]
    )
    checkAll("Bifunctor[IndexedStateT[ListWrapper, Int, *, *]]",
             SerializableTests.serializable(Bifunctor[IndexedStateT[ListWrapper, Int, *, *]])
    )

    Bifunctor[IndexedStateT[ListWrapper, Int, *, *]]
  }

  {
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    implicit val FS: Profunctor[IndexedStateT[ListWrapper, *, *, Int]] =
      IndexedStateT.catsDataProfunctorForIndexedStateT

    checkAll("IndexedStateT[ListWrapper, String, Int, Int]",
             ProfunctorTests[IndexedStateT[ListWrapper, *, *, Int]].profunctor[MiniInt, String, String, Int, Int, Int]
    )
    checkAll("Profunctor[IndexedStateT[ListWrapper, *, *, Int]]",
             SerializableTests.serializable(Profunctor[IndexedStateT[ListWrapper, *, *, Int]])
    )

    Profunctor[IndexedStateT[ListWrapper, *, *, Int]]
  }

  {
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    implicit val FS: Strong[IndexedStateT[ListWrapper, *, *, Int]] = IndexedStateT.catsDataStrongForIndexedStateT

    checkAll("IndexedStateT[ListWrapper, *, *, Int]",
             StrongTests[IndexedStateT[ListWrapper, *, *, Int]].strong[MiniInt, Int, Boolean, Boolean, Boolean, String]
    )
    checkAll("Strong[IndexedStateT[ListWrapper, *, *, Int]]",
             SerializableTests.serializable(Strong[IndexedStateT[ListWrapper, *, *, Int]])
    )

    Strong[IndexedStateT[ListWrapper, *, *, Int]]
  }

  {
    // F has a Monad
    implicit val F: Monad[ListWrapper] = ListWrapper.monad

    checkAll("IndexedStateT[ListWrapper, MiniInt, Int, *]",
             MonadTests[IndexedStateT[ListWrapper, MiniInt, MiniInt, *]].monad[Int, Int, Int]
    )
    checkAll("Monad[IndexedStateT[ListWrapper, Int, Int, *]]",
             SerializableTests.serializable(Monad[IndexedStateT[ListWrapper, Int, Int, *]])
    )

    Monad[IndexedStateT[ListWrapper, Int, Int, *]]
    FlatMap[IndexedStateT[ListWrapper, Int, Int, *]]
    Applicative[IndexedStateT[ListWrapper, Int, Int, *]]
    Apply[IndexedStateT[ListWrapper, Int, Int, *]]
    Functor[IndexedStateT[ListWrapper, Int, Int, *]]
  }

  {
    // F has a Monad and a SemigroupK
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    implicit val S: SemigroupK[ListWrapper] = ListWrapper.semigroupK

    checkAll("IndexedStateT[ListWrapper, MiniInt, Int, *]",
             SemigroupKTests[IndexedStateT[ListWrapper, MiniInt, Int, *]].semigroupK[Int]
    )
    checkAll("SemigroupK[IndexedStateT[ListWrapper, Int, *]]",
             SerializableTests.serializable(SemigroupK[IndexedStateT[ListWrapper, String, Int, *]])
    )
  }

  {
    // F has an Alternative
    implicit val G: Monad[ListWrapper] = ListWrapper.monad
    implicit val F: Alternative[ListWrapper] = ListWrapper.alternative
    val SA =
      IndexedStateT
        .catsDataAlternativeForIndexedStateT[ListWrapper, MiniInt](ListWrapper.monad, ListWrapper.alternative)

    implicit val f: Isomorphisms[IndexedStateT[ListWrapper, MiniInt, MiniInt, *]] = Isomorphisms.invariant(SA)

    checkAll("IndexedStateT[ListWrapper, MiniInt, Int, Int]",
             AlternativeTests[IndexedStateT[ListWrapper, MiniInt, MiniInt, *]](using SA).alternative[Int, Int, Int]
    )
    checkAll("Alternative[IndexedStateT[ListWrapper, Int, Int, *]]", SerializableTests.serializable(SA))

    Monad[IndexedStateT[ListWrapper, Int, Int, *]]
    FlatMap[IndexedStateT[ListWrapper, Int, Int, *]]
    Alternative[IndexedStateT[ListWrapper, Int, Int, *]]
    Applicative[IndexedStateT[ListWrapper, Int, Int, *]]
    Apply[IndexedStateT[ListWrapper, Int, Int, *]]
    // Functor[IndexedStateT[ListWrapper, Int, Int, *]]
    MonoidK[IndexedStateT[ListWrapper, Int, Int, *]]
    SemigroupK[IndexedStateT[ListWrapper, Int, Int, *]]
  }

  {
    implicit val iso: Isomorphisms[State[MiniInt, *]] = Isomorphisms.invariant[State[MiniInt, *]]

    checkAll("State[MiniInt, *]", MonadTests[State[MiniInt, *]].monad[Int, Int, Int])
    checkAll("Monad[State[Long, *]]", SerializableTests.serializable(Monad[State[Long, *]]))
  }

  {
    // F has a MonadError
    implicit val iso: Isomorphisms[StateT[Option, MiniInt, *]] = Isomorphisms.invariant[StateT[Option, MiniInt, *]]
    implicit val eqEitherTFA: Eq[EitherT[StateT[Option, MiniInt, *], Unit, Int]] =
      EitherT.catsDataEqForEitherT[StateT[Option, MiniInt, *], Unit, Int]

    checkAll("StateT[Option, MiniInt, Int]",
             MonadErrorTests[StateT[Option, MiniInt, *], Unit].monadError[Int, Int, Int]
    )
    checkAll("MonadError[StateT[Option, Int, *], Unit]",
             SerializableTests.serializable(MonadError[StateT[Option, Int, *], Unit])
    )
  }

}

object IndexedStateTSuite extends IndexedStateTSuiteInstances {
  implicit def stateEq[S: Eq: ExhaustiveCheck, A: Eq]: Eq[State[S, A]] =
    indexedStateTEq[Eval, S, S, A]

  val add1: State[Int, Int] = State(n => (n + 1, n))
}

sealed trait IndexedStateTSuiteInstances {

  implicit def indexedStateTEq[F[_], SA, SB, A](implicit
    SA: ExhaustiveCheck[SA],
    FSB: Eq[F[(SB, A)]],
    F: FlatMap[F]
  ): Eq[IndexedStateT[F, SA, SB, A]] =
    Eq.by[IndexedStateT[F, SA, SB, A], SA => F[(SB, A)]](state => s => state.run(s))
}
