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
import cats.arrow.{Profunctor, Strong}
import cats.data.{EitherT, IRWST, IndexedReaderWriterStateT, ReaderWriterState, ReaderWriterStateT}
import cats.kernel.{Eq, Monoid}
import cats.laws.discipline.*
import cats.laws.discipline.eq.*
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.syntax.apply.*
import cats.syntax.semigroup.*
import cats.syntax.traverse.*
import org.scalacheck.Arbitrary
import cats.syntax.eq.*
import org.scalacheck.Prop.*

class ReaderWriterStateTSuite extends CatsSuite {
  import ReaderWriterStateTSuite.*

  test("Basic ReaderWriterState usage") {
    forAll { (context: String, initial: Int) =>
      val (log, state, result) = addAndLog(5).run(context, initial).value

      assert(log === (Vector(s"${context}: Added 5")))
      assert(state === (initial + 5))
      assert(result === (initial + 5))
    }
  }

  test("Traversing with ReaderWriterState is stack-safe") {
    val ns = (0 to 70000).toList
    val rws = ns.traverse(_ => addLogUnit(1))

    assert(rws.runS("context", 0).value === 70001)
  }

  test("flatMap is stack-safe on repeated left binds when F is") {
    val ns = (0 to 70000).toList
    val one = addLogUnit(1)
    val rws = ns.foldLeft(one)((acc, _) => acc.flatMap(_ => one))

    assert(rws.runS("context", 0).value === 70002)
  }

  test("flatMap is stack-safe on repeated right binds when F is") {
    val ns = (0 to 70000).toList
    val one = addLogUnit(1)
    val rws = ns.foldLeft(one)((acc, _) => one.flatMap(_ => acc))

    assert(rws.runS("context", 0).value === 70002)
  }

  test("map2 combines logs") {
    forAll {
      (rwsa: ReaderWriterState[String, Vector[Int], Int, Int],
       rwsb: ReaderWriterState[String, Vector[Int], Int, Int],
       c: String,
       s: Int
      ) =>
        val logMap2 = rwsa.map2(rwsb)((_, _) => ()).runL(c, s).value

        val (logA, stateA, _) = rwsa.run(c, s).value
        val logB = rwsb.runL(c, stateA).value
        val combinedLog = logA |+| logB

        assert(logMap2 === combinedLog)
    }
  }

  test("ReaderWriterState.ask provides the context") {
    forAll { (context: String, initial: Int) =>
      assert(ReaderWriterState.ask[String, String, Int].runA(context, initial).value === context)
    }
  }

  test("ReaderWriterState.pure, ReaderWriterStateT.pure and IndexedReaderWriterStateT.pure are consistent") {
    forAll { (context: String, value: Int) =>
      val rws: ReaderWriterState[String, Vector[String], Int, Int] = ReaderWriterState.pure(value)
      val rwst: ReaderWriterState[String, Vector[String], Int, Int] = ReaderWriterStateT.pure(value)
      val irwst: ReaderWriterState[String, Vector[String], Int, Int] = IndexedReaderWriterStateT.pure(value)

      assert(rws.run(context, value) === (rwst.run(context, value)))
      assert(rwst.run(context, value) === (irwst.run(context, value)))
    }
  }

  test("ReaderWriterState.pure creates an ReaderWriterState with an empty log") {
    forAll { (context: String, initial: Int) =>
      val rws: ReaderWriterState[String, String, Int, Unit] = ReaderWriterState.pure(())
      assert(rws.run(context, initial).value === ((Monoid[String].empty, initial, ())))
    }
  }

  test("ReaderWriterState.get, ReaderWriterStateT.get and IndexedReaderWriterStateT.get are consistent") {
    forAll { (context: String, initial: Int) =>
      val rws: ReaderWriterState[String, Vector[String], Int, Int] = ReaderWriterState.get
      val rwst: ReaderWriterState[String, Vector[String], Int, Int] = ReaderWriterStateT.get
      val irwst: ReaderWriterState[String, Vector[String], Int, Int] = IndexedReaderWriterStateT.get

      assert(rws.run(context, initial) === (rwst.run(context, initial)))
      assert(rwst.run(context, initial) === (irwst.run(context, initial)))
    }
  }

  test("ReaderWriterState.get and instance get are consistent") {
    forAll { (context: String, initial: Int) =>
      val singleton = ReaderWriterState.get[String, String, Int]
      val instance = ReaderWriterState.pure[String, String, Int, Unit](()).get

      assert(singleton.run(context, initial) === (instance.run(context, initial)))
    }
  }

  test("ReaderWriterState.inspect and instance inspect are consistent") {
    forAll { (context: String, initial: Int) =>
      val singleton = ReaderWriterState.inspect[String, String, Int, String](_.toString)
      val instance = ReaderWriterState.pure[String, String, Int, Unit](()).inspect(_.toString)

      assert(singleton.run(context, initial) === (instance.run(context, initial)))
    }
  }

  test("ReaderWriterState.inspect, ReaderWriterStateT.inspect and IndexedReaderWriterStateT.inspect are consistent") {
    forAll { (context: String, initial: Int, f: Int => Int) =>
      val rws: ReaderWriterState[String, Vector[String], Int, Int] = ReaderWriterState.inspect(f)
      val rwst: ReaderWriterState[String, Vector[String], Int, Int] = ReaderWriterStateT.inspect(f)
      val irwst: ReaderWriterState[String, Vector[String], Int, Int] = IndexedReaderWriterStateT.inspect(f)

      assert(rws.run(context, initial) === (rwst.run(context, initial)))
      assert(rwst.run(context, initial) === (irwst.run(context, initial)))
    }
  }

  test("ReaderWriterState.inspect, ReaderWriterStateT.inspectF and IndexedReaderWriterStateT.inspectF are consistent") {
    forAll { (context: String, initial: Int, f: Int => Int) =>
      val rws: ReaderWriterState[String, String, Int, Int] = ReaderWriterState.inspect(f)
      val rwst: ReaderWriterState[String, String, Int, Int] = ReaderWriterStateT.inspectF(f.andThen(Eval.now))
      val irwst: ReaderWriterState[String, String, Int, Int] = IndexedReaderWriterStateT.inspectF(f.andThen(Eval.now))

      assert(rws.run(context, initial) === (rwst.run(context, initial)))
      assert(rwst.run(context, initial) === (irwst.run(context, initial)))
    }
  }

  test("ReaderWriterState.modify, ReaderWriterStateT.modify and IndexedReaderWriterStateT.modify are consistent") {
    forAll { (context: String, initial: Int, f: Int => Int) =>
      val rws: ReaderWriterState[String, Vector[String], Int, Unit] = ReaderWriterState.modify(f)
      val rwst: ReaderWriterState[String, Vector[String], Int, Unit] = ReaderWriterStateT.modify(f)
      val irwst: ReaderWriterState[String, Vector[String], Int, Unit] = IndexedReaderWriterStateT.modify(f)

      assert(rws.run(context, initial) === (rwst.run(context, initial)))
      assert(rwst.run(context, initial) === (irwst.run(context, initial)))
    }
  }

  test("ReaderWriterState.modify, ReaderWriterStateT.modifyF and IndexedReaderWriterStateT.modifyF are consistent") {
    forAll { (context: String, initial: Int, f: Int => Int) =>
      val rws: ReaderWriterState[String, Vector[String], Int, Unit] = ReaderWriterState.modify(f)
      val rwst: ReaderWriterState[String, Vector[String], Int, Unit] = ReaderWriterStateT.modifyF(f.andThen(Eval.now))
      val irwst: ReaderWriterState[String, Vector[String], Int, Unit] =
        IndexedReaderWriterStateT.modifyF(f.andThen(Eval.now))

      assert(rws.run(context, initial) === (rwst.run(context, initial)))
      assert(rwst.run(context, initial) === (irwst.run(context, initial)))
    }
  }

  test("ReaderWriterState.pure, ReaderWriterStateT.liftF and IndexedReaderWriterStateT.liftF are consistent") {
    forAll { (context: String, initial: Int, value: Int) =>
      val rws: ReaderWriterState[String, Vector[String], Int, Int] = ReaderWriterState.pure(value)
      val rwst: ReaderWriterState[String, Vector[String], Int, Int] = ReaderWriterStateT.liftF(Eval.now(value))
      val irwst: ReaderWriterState[String, Vector[String], Int, Int] = IndexedReaderWriterStateT.liftF(Eval.now(value))

      assert(rws.run(context, initial) === (rwst.run(context, initial)))
      assert(rwst.run(context, initial) === (irwst.run(context, initial)))
    }
  }

  test("ReaderWriterState.set, ReaderWriterStateT.set and IndexedReaderWriterStateT.set are consistent") {
    forAll { (context: String, initial: Int, next: Int) =>
      val rws: ReaderWriterState[String, Vector[String], Int, Unit] = ReaderWriterState.set(next)
      val rwst: ReaderWriterState[String, Vector[String], Int, Unit] = ReaderWriterStateT.set(next)
      val irwst: ReaderWriterState[String, Vector[String], Int, Unit] = IndexedReaderWriterStateT.set(next)

      assert(rws.run(context, initial) === (rwst.run(context, initial)))
      assert(rwst.run(context, initial) === (irwst.run(context, initial)))
    }
  }

  test("ReaderWriterState.set, ReaderWriterStateT.setF and IndexedReaderWriterStateT.setF are consistent") {
    forAll { (context: String, initial: Int, next: Int) =>
      val rws: ReaderWriterState[String, Vector[String], Int, Unit] = ReaderWriterState.set(next)
      val rwst: ReaderWriterState[String, Vector[String], Int, Unit] = ReaderWriterStateT.setF(Eval.now(next))
      val irwst: ReaderWriterState[String, Vector[String], Int, Unit] = IndexedReaderWriterStateT.setF(Eval.now(next))

      assert(rws.run(context, initial) === (rwst.run(context, initial)))
      assert(rwst.run(context, initial) === (irwst.run(context, initial)))
    }
  }

  test("ReaderWriterState.tell, ReaderWriterStateT.tell and IndexedReaderWriterStateT.tell are consistent") {
    forAll { (context: String, initial: Int, log: String) =>
      val rws: ReaderWriterState[String, String, Int, Unit] = ReaderWriterState.tell(log)
      val rwst: ReaderWriterState[String, String, Int, Unit] = ReaderWriterStateT.tell(log)
      val irwst: ReaderWriterState[String, String, Int, Unit] = IndexedReaderWriterStateT.tell(log)

      assert(rws.run(context, initial) === (rwst.run(context, initial)))
      assert(rwst.run(context, initial) === (irwst.run(context, initial)))
    }
  }

  test("ReaderWriterState.tell, ReaderWriterStateT.tellF and IndexedReaderWriterStateT.tellF are consistent") {
    forAll { (context: String, initial: Int, log: String) =>
      val rws: ReaderWriterState[String, String, Int, Unit] = ReaderWriterState.tell(log)
      val rwst: ReaderWriterState[String, String, Int, Unit] = ReaderWriterStateT.tellF(Eval.now(log))
      val irwst: ReaderWriterState[String, String, Int, Unit] = IndexedReaderWriterStateT.tellF(Eval.now(log))

      assert(rws.run(context, initial) === (rwst.run(context, initial)))
      assert(rwst.run(context, initial) === (irwst.run(context, initial)))
    }
  }

  test("ReaderWriterState.tell + written is identity") {
    forAll { (context: String, initial: Int, log: String) =>
      assert(ReaderWriterState.tell[String, String, Int](log).written.runA(context, initial).value === log)
    }
  }

  test("Semigroupal syntax is usable on ReaderWriterState") {
    val rws = addAndLog(5) *> addAndLog(10)
    val (log, state, result) = rws.run("context", 0).value

    assert(log === (Vector("context: Added 5", "context: Added 10")))
    assert(state === 15)
    assert(result === 15)
  }

  test("flatMap and flatMapF+tell are consistent") {
    forAll {
      (rwst: ReaderWriterStateT[Option, String, String, String, Int],
       f: Int => Option[Int],
       initial: String,
       context: String,
       log: String
      ) =>
        val flatMap = rwst.flatMap { a =>
          ReaderWriterStateT { (e, s) =>
            f(a).map((log, s, _))
          }
        }

        val flatMapF = rwst.flatMapF(f).tell(log)

        assert(flatMap.run(context, initial) === (flatMapF.run(context, initial)))
    }
  }

  test("runEmpty, runEmptyS, runEmptyA and runEmptyL are consistent") {
    forAll { (f: ReaderWriterStateT[Option, String, String, String, Int], c: String) =>
      assert((f.runEmptyL(c), f.runEmptyS(c), f.runEmptyA(c)).tupled === (f.runEmpty(c)))
    }
  }

  test("reset on pure is a noop") {
    forAll { (c: String, s: Int, a: Int) =>
      val pure = ReaderWriterState.pure[String, String, Int, Int](a)
      assert(pure.reset.run(c, s) === (pure.run(c, s)))
    }
  }

  test("modify identity is a noop") {
    forAll { (f: ReaderWriterStateT[Option, String, String, String, Int], c: String, initial: String) =>
      assert(f.modify(identity).run(c, initial) === (f.run(c, initial)))
    }
  }

  test("modify modifies only the state") {
    forAll { (rws: ReaderWriterStateT[Option, String, String, Long, Long], c: String, f: Long => Long, initial: Long) =>
      assert(rws.modify(f).runS(c, initial) === (rws.runS(c, initial).map(f)))
      assert(rws.modify(f).runA(c, initial) === (rws.runA(c, initial)))
    }
  }

  test("reset modifies only the log") {
    forAll { (rws: ReaderWriterState[String, String, Int, Int], c: String, s: Int) =>
      assert(rws.reset.runA(c, s) === (rws.runA(c, s)))
      assert(rws.reset.runS(c, s) === (rws.runS(c, s)))
    }
  }

  test("modify is equivalent to get and set") {
    forAll { (c: String, f: Long => Long, initial: Long) =>
      val s1 = ReaderWriterStateT.modify[Option, String, String, Long](f)
      val s2 = for {
        l <- ReaderWriterStateT.get[Option, String, String, Long]
        _ <- ReaderWriterStateT.set[Option, String, String, Long](f(l))
      } yield ()

      assert(s1.run(c, initial) === (s2.run(c, initial)))
    }
  }

  test("ReaderWriterStateT.set is equivalent to modify ignoring first param") {
    forAll { (c: String, initial: Long, s: Long) =>
      val s1 = ReaderWriterStateT.set[Option, String, String, Long](s)
      val s2 = ReaderWriterStateT.modify[Option, String, String, Long](_ => s)

      assert(s1.run(c, initial) === (s2.run(c, initial)))
    }
  }

  test("ReaderWriterStateT.setF is equivalent to modifyF ignoring first param") {
    forAll { (c: String, initial: Long, s: Option[Long]) =>
      val s1 = ReaderWriterStateT.setF[Option, String, String, Long](s)
      val s2 = ReaderWriterStateT.modifyF[Option, String, String, Long](_ => s)

      assert(s1.run(c, initial) === (s2.run(c, initial)))
    }
  }

  test("ReaderWriterStateT.mapK transforms effect") {
    val f: Eval ~> Id = new (Eval ~> Id) { def apply[A](a: Eval[A]): A = a.value }
    forAll { (state: ReaderWriterStateT[Eval, Long, String, String, Int], env: Long, initial: String) =>
      assert(state.mapK(f).runA(env, initial) === (state.runA(env, initial).value))
    }
  }

  test(".get and then .run produces the same state as value") {
    forAll { (c: String, initial: Long, rws: ReaderWriterState[String, String, Long, Long]) =>
      val (_, state, value) = rws.get.run(c, initial).value

      assert(state === value)
    }
  }

  test(".get and .flatMap with .get are equivalent") {
    forAll { (c: String, initial: Long, rws: ReaderWriterState[String, String, Long, Long]) =>
      assert(rws.get.run(c, initial) === (rws.flatMap(_ => ReaderWriterState.get).run(c, initial)))
    }
  }

  test("pure + listen + map(_._1) + runEmptyA is identity") {
    forAll { (i: Int) =>
      assert(
        IndexedReaderWriterStateT
          .pure[Id, String, String, Int, Int](i)
          .listen
          .map(_._1)
          .runEmptyA("") === i
      )
    }
  }

  test("tell + listen + map(_._2) + runEmptyA is identity") {
    forAll { (s: String) =>
      assert(
        IndexedReaderWriterStateT
          .tell[Id, String, String, Int](s)
          .listen
          .map(_._2)
          .runEmptyA("") === s
      )
    }
  }

  implicit val iso: Isomorphisms[IndexedReaderWriterStateT[ListWrapper, String, String, Int, String, *]] =
    Isomorphisms.invariant[IndexedReaderWriterStateT[ListWrapper, String, String, Int, String, *]](
      IndexedReaderWriterStateT.catsDataFunctorForIRWST(ListWrapper.functor)
    )

  checkAll(
    "IndexedReaderWriterStateT[Eval, Boolean, String, MiniInt, String, *]",
    DeferTests[IndexedReaderWriterStateT[Eval, Boolean, String, MiniInt, String, *]].defer[Int]
  )

  {
    implicit val F: Monad[ListWrapper] = ListWrapper.monad

    checkAll(
      "IndexedReaderWriterStateT[ListWrapper, Boolean, String, MiniInt, String, *]",
      FunctorTests[IndexedReaderWriterStateT[ListWrapper, Boolean, String, MiniInt, String, *]].functor[Int, Int, Int]
    )
    checkAll(
      "Functor[IndexedReaderWriterStateT[ListWrapper, String, String, Int, String, *]]",
      SerializableTests.serializable(Functor[IndexedReaderWriterStateT[ListWrapper, String, String, Int, String, *]])
    )

    checkAll(
      "IndexedReaderWriterStateT[ListWrapper, String, String, *, Int, Int]",
      ContravariantTests[IndexedReaderWriterStateT[ListWrapper, Boolean, String, *, Int, Int]]
        .contravariant[MiniInt, String, Boolean]
    )
    checkAll(
      "Contravariant[IndexedReaderWriterStateT[ListWrapper, String, String, *, Int, Int]]",
      SerializableTests.serializable(Contravariant[IndexedReaderWriterStateT[ListWrapper, String, String, *, Int, Int]])
    )

    checkAll(
      "IndexedReaderWriterStateT[ListWrapper, String, String, *, *, Int]",
      ProfunctorTests[IndexedReaderWriterStateT[ListWrapper, Boolean, String, *, *, Int]]
        .profunctor[MiniInt, Int, Int, String, String, String]
    )
    checkAll(
      "Profunctor[IndexedReaderWriterStateT[ListWrapper, String, String, *, *, Int]]",
      SerializableTests.serializable(Profunctor[IndexedReaderWriterStateT[ListWrapper, String, String, *, *, Int]])
    )

    checkAll(
      "IndexedReaderWriterStateT[ListWrapper, Boolean, String, *, *, Int]",
      StrongTests[IndexedReaderWriterStateT[ListWrapper, Boolean, String, *, *, Int]]
        .strong[MiniInt, Int, Boolean, Boolean, Boolean, String]
    )
    checkAll(
      "Strong[IndexedReaderWriterStateT[ListWrapper, String, String, *, *, Int]]",
      SerializableTests.serializable(Strong[IndexedReaderWriterStateT[ListWrapper, String, String, *, *, Int]])
    )

    checkAll(
      "IndexedReaderWriterStateT[ListWrapper, String, String, Int, *, *]",
      BifunctorTests[IndexedReaderWriterStateT[ListWrapper, Boolean, String, MiniInt, *, *]]
        .bifunctor[Int, Int, Boolean, String, String, String]
    )
    checkAll(
      "Bifunctor[IndexedReaderWriterStateT[ListWrapper, String, String, Int, *, *]]",
      SerializableTests.serializable(Bifunctor[IndexedReaderWriterStateT[ListWrapper, String, String, Int, *, *]])
    )
  }

  {
    implicit val G: Monad[ListWrapper] = ListWrapper.monad

    val SA = IRWST.catsDataAlternativeForIRWST[ListWrapper, Boolean, String, MiniInt](ListWrapper.monad,
                                                                                      ListWrapper.alternative,
                                                                                      Monoid[String]
    )

    checkAll(
      "IndexedReaderWriterStateT[ListWrapper, String, String, Int, Int, *]",
      AlternativeTests[IRWST[ListWrapper, Boolean, String, MiniInt, MiniInt, *]](using SA).alternative[Int, Int, Int]
    )
    checkAll("Alternative[IndexedReaderWriterStateT[ListWrapper, String, String, Int, Int, *]]",
             SerializableTests.serializable(SA)
    )
  }

  {
    implicit val LWM: Monad[ListWrapper] = ListWrapper.monad

    checkAll(
      "ReaderWriterStateT[ListWrapper, Boolean, String, MiniInt, MiniInt, *]",
      MonadTests[ReaderWriterStateT[ListWrapper, Boolean, String, MiniInt, *]].monad[Int, Int, Int]
    )
    checkAll(
      "Monad[ReaderWriterStateT[ListWrapper, String, String, Int, *]]",
      SerializableTests.serializable(Monad[ReaderWriterStateT[ListWrapper, String, String, Int, *]])
    )
  }

  {
    implicit val iso: Isomorphisms[ReaderWriterStateT[Option, Boolean, String, MiniInt, *]] =
      Isomorphisms.invariant[ReaderWriterStateT[Option, Boolean, String, MiniInt, *]]
    implicit val eqEitherTFA: Eq[EitherT[ReaderWriterStateT[Option, Boolean, String, MiniInt, *], Unit, Int]] =
      EitherT.catsDataEqForEitherT[ReaderWriterStateT[Option, Boolean, String, MiniInt, *], Unit, Int]

    checkAll(
      "ReaderWriterStateT[Option, Boolean, String, MiniIntInt, *]",
      MonadErrorTests[ReaderWriterStateT[Option, Boolean, String, MiniInt, *], Unit].monadError[Int, Int, Int]
    )
    checkAll(
      "MonadError[ReaderWriterStateT[Option, String, String, Int, *], Unit]",
      SerializableTests.serializable(MonadError[ReaderWriterStateT[Option, String, String, Int, *], Unit])
    )
  }

  {
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    implicit val S: SemigroupK[ListWrapper] = ListWrapper.semigroupK

    checkAll(
      "ReaderWriterStateT[ListWrapper, Boolean, String, MiniInt, *]",
      SemigroupKTests[ReaderWriterStateT[ListWrapper, Boolean, String, MiniInt, *]].semigroupK[Int]
    )
    checkAll(
      "SemigroupK[ReaderWriterStateT[ListWrapper, String, String, Int, *]]",
      SerializableTests.serializable(SemigroupK[ReaderWriterStateT[ListWrapper, String, String, Int, *]])
    )
  }

}

object ReaderWriterStateTSuite {
  def addAndLog(i: Int): ReaderWriterState[String, Vector[String], Int, Int] =
    ReaderWriterState { (context, state) =>
      (Vector(s"${context}: Added ${i}"), state + i, state + i)
    }

  def addLogUnit(i: Int): ReaderWriterState[String, Unit, Int, Int] =
    ReaderWriterState { (context, state) =>
      ((), state + i, state + i)
    }

  implicit def IRWSTEq[F[_], E, L, SA, SB, A](implicit
    SA: ExhaustiveCheck[SA],
    SB: Arbitrary[SB],
    E: ExhaustiveCheck[E],
    FLSB: Eq[F[(L, SB, A)]],
    F: Monad[F]
  ): Eq[IndexedReaderWriterStateT[F, E, L, SA, SB, A]] =
    Eq.by[IndexedReaderWriterStateT[F, E, L, SA, SB, A], (E, SA) => F[(L, SB, A)]] { state => (e, s) =>
      state.run(e, s)
    }
}
