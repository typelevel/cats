package cats
package tests

import cats.data.{ ReaderWriterStateT, ReaderWriterState, EitherT }
import cats.functor.{ Bifunctor, Contravariant, Profunctor }
import cats.laws.discipline._
import cats.laws.discipline.eq._
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary

class ReaderWriterStateTTests extends CatsSuite {
  import ReaderWriterStateTTests._

  test("Basic ReaderWriterState usage") {
    forAll { (context: String, initial: Int) =>
      val (log, state, result) = addAndLog(5).run(context, initial).value

      log should === (Vector(s"${context}: Added 5"))
      state should === (initial + 5)
      result should === (initial + 5)
    }
  }

  test("Traversing with ReaderWriterState is stack-safe") {
    val ns = (0 to 70000).toList
    val rws = ns.traverse(_ => addLogUnit(1))

    rws.runS("context", 0).value should === (70001)
  }

  test("map2 combines logs") {
    forAll { (rwsa: ReaderWriterState[String, Int, Vector[Int], Int], rwsb: ReaderWriterState[String, Int, Vector[Int], Int], c: String, s: Int) =>
      val logMap2 = rwsa.map2(rwsb)((_, _) => ()).runL(c, s).value

      val (logA, stateA, _) = rwsa.run(c, s).value
      val logB = rwsb.runL(c, stateA).value
      val combinedLog = logA |+| logB

      logMap2 should === (combinedLog)
    }
  }

  test("ReaderWriterState.ask provides the context") {
    forAll { (context: String, initial: Int) =>
      ReaderWriterState.ask[String, Int, String].runA(context, initial).value should === (context)
    }
  }

  test("local is consistent with contramap") {
    forAll { (context: Int, initial: Int, f: Int => String) =>
      val rwsa = ReaderWriterState.pure[String, Int, Unit, Unit](()).contramap(f).flatMap(_ => ReaderWriterState.ask)
      val rwsb = ReaderWriterState.pure[String, Int, Unit, Unit](()).local(f).flatMap(_ => ReaderWriterState.ask)

      rwsa.runA(context, initial) should === (rwsb.runA(context, initial))
    }
  }

  test("ReaderWriterState.pure and ReaderWriterStateT.pure are consistent") {
    forAll { (value: Int) =>
      val rws: ReaderWriterState[String, Int, Vector[String], Int] = ReaderWriterState.pure(value)
      val rwst: ReaderWriterState[String, Int, Vector[String], Int] = ReaderWriterStateT.pure(value)

      rws should === (rwst)
    }
  }

  test("ReaderWriterState.pure creates an ReaderWriterState with an empty log") {
    forAll { (context: String, initial: Int) =>
      val rws: ReaderWriterState[String, Int, String, Unit] = ReaderWriterState.pure(())
      rws.run(context, initial).value should === ((Monoid[String].empty, initial, ()))
    }
  }

  test("ReaderWriterState.get and ReaderWriterStateT.get are consistent") {
    forAll { (initial: Int) =>
      val rws: ReaderWriterState[String, Int, Vector[String], Int] = ReaderWriterState.get
      val rwst: ReaderWriterState[String, Int, Vector[String], Int] = ReaderWriterStateT.get

      rws should === (rwst)
    }
  }

  test("ReaderWriterState.get and instance get are consistent") {
    forAll { (initial: Int) =>
      val singleton = ReaderWriterState.inspect[String, Int, String, String](_.toString)
      val instance = ReaderWriterState.pure[String, Int, String, Unit](()).inspect(_.toString)

      singleton should === (instance)
    }
  }

  test("ReaderWriterState.inspect and ReaderWriterStateT.inspect are consistent") {
    forAll { (f: Int => Int) =>
      val rws: ReaderWriterState[String, Int, Vector[String], Int] = ReaderWriterState.inspect(f)
      val rwst: ReaderWriterState[String, Int, Vector[String], Int] = ReaderWriterStateT.inspect(f)

      rws should === (rwst)
    }
  }

  test("ReaderWriterState.inspect and ReaderWriterStateT.inspectF are consistent") {
    forAll { (f: Int => Int) =>
      val rws: ReaderWriterState[String, Int, String, Int] = ReaderWriterState.inspect(f)
      val rwst: ReaderWriterState[String, Int, String, Int] = ReaderWriterStateT.inspectF(f.andThen(Eval.now))

      rws should === (rwst)
    }
  }

  test("ReaderWriterState.modify and ReaderWriterStateT.modify are consistent") {
    forAll { (f: Int => Int) =>
      val rws: ReaderWriterState[String, Int, Vector[String], Unit] = ReaderWriterState.modify(f)
      val rwst: ReaderWriterState[String, Int, Vector[String], Unit] = ReaderWriterStateT.modify(f)

      rws should === (rwst)
    }
  }

  test("ReaderWriterState.modify and ReaderWriterStateT.modifyF are consistent") {
    forAll { (f: Int => Int) =>
      val rws: ReaderWriterState[String, Int, Vector[String], Unit] = ReaderWriterState.modify(f)
      val rwst: ReaderWriterState[String, Int, Vector[String], Unit] = ReaderWriterStateT.modifyF(f.andThen(Eval.now))

      rws should === (rwst)
    }
  }

  test("ReaderWriterState.pure and ReaderWriterStateT.lift are consistent") {
    forAll { (value: Int) =>
      val rws: ReaderWriterState[String, Int, Vector[String], Int] = ReaderWriterState.pure(value)
      val rwst: ReaderWriterState[String, Int, Vector[String], Int] = ReaderWriterStateT.lift(Eval.now(value))

      rws should === (rwst)
    }
  }

  test("ReaderWriterState.set and ReaderWriterStateT.set are consistent") {
    forAll { (next: Int) =>
      val rws: ReaderWriterState[String, Int, Vector[String], Unit] = ReaderWriterState.set(next)
      val rwst: ReaderWriterState[String, Int, Vector[String], Unit] = ReaderWriterStateT.set(next)

      rws should === (rwst)
    }
  }

  test("ReaderWriterState.set and ReaderWriterStateT.setF are consistent") {
    forAll { (next: Int) =>
      val rws: ReaderWriterState[String, Int, Vector[String], Unit] = ReaderWriterState.set(next)
      val rwst: ReaderWriterState[String, Int, Vector[String], Unit] = ReaderWriterStateT.setF(Eval.now(next))

      rws should === (rwst)
    }
  }

  test("ReaderWriterState.tell and ReaderWriterStateT.tell are consistent") {
    forAll { (log: String) =>
      val rws: ReaderWriterState[String, Int, String, Unit] = ReaderWriterState.tell(log)
      val rwst: ReaderWriterState[String, Int, String, Unit] = ReaderWriterStateT.tell(log)

      rws should === (rwst)
    }
  }

  test("ReaderWriterState.tell and ReaderWriterStateT.tellF are consistent") {
    forAll { (log: String) =>
      val rws: ReaderWriterState[String, Int, String, Unit] = ReaderWriterState.tell(log)
      val rwst: ReaderWriterState[String, Int, String, Unit] = ReaderWriterStateT.tellF(Eval.now(log))

      rws should === (rwst)
    }
  }

  test("ReaderWriterState.tell + written is identity") {
    forAll { (context: String, initial: Int, log: String) =>
      ReaderWriterState.tell[String, Int, String](log).written.runA(context, initial).value should === (log)
    }
  }

  test("Cartesian syntax is usable on ReaderWriterState") {
    val rws = addAndLog(5) *> addAndLog(10)
    val (log, state, result) = rws.run("context", 0).value

    log should === (Vector("context: Added 5", "context: Added 10"))
    state should === (15)
    result should === (15)
  }

  test("flatMap and flatMapF+tell are consistent") {
    forAll {
      (rwst: ReaderWriterStateT[Option, String, String, String, Int], f: Int => Option[Int],
        initial: String, context: String, log: String) =>

      val flatMap = rwst.flatMap { a =>
        ReaderWriterStateT { (e, s) =>
          f(a).map((log, s, _))
        }
      }

      val flatMapF = rwst.flatMapF(f).tell(log)

      flatMap.run(context, initial) should === (flatMapF.run(context, initial))
    }
  }

  test("runEmpty, runEmptyS, runEmptyA and runEmptyL are consistent") {
    forAll { (f: ReaderWriterStateT[Option, String, String, String, Int], c: String) =>
      (f.runEmptyL(c), f.runEmptyS(c), f.runEmptyA(c)).tupled should === (f.runEmpty(c))
    }
  }

  test("reset on pure is a noop") {
    forAll { (c: String, s: Int, a: Int) =>
      val pure = ReaderWriterState.pure[String, Int, String, Int](a)
      pure.reset should === (pure)
    }
  }

  test("modify identity is a noop") {
    forAll { (f: ReaderWriterStateT[Option, String, String, String, Int], c: String, initial: String) =>
      f.modify(identity).run(c, initial) should === (f.run(c, initial))
    }
  }

  test("modify modifies only the state") {
    forAll { (rws: ReaderWriterStateT[Option, String, Long, String, Long], c: String, f: Long => Long, initial: Long) =>
      rws.modify(f).runS(c, initial) should === (rws.runS(c, initial).map(f))
      rws.modify(f).runA(c, initial) should === (rws.runA(c, initial))
    }
  }

  test("reset modifies only the log") {
    forAll { (rws: ReaderWriterState[String, Int, String, Int], c: String, s: Int) =>
      rws.reset.runA(c, s) should === (rws.runA(c, s))
      rws.reset.runS(c, s) should === (rws.runS(c, s))
    }
  }

  test("modify is equivalent to get and set") {
    forAll { (c: String, f: Long => Long, initial: Long) =>
      val s1 = ReaderWriterStateT.modify[Option, String, Long, String](f)
      val s2 = for {
        l <- ReaderWriterStateT.get[Option, String, Long, String]
        _ <- ReaderWriterStateT.set[Option, String, Long, String](f(l))
      } yield ()

      s1.run(c, initial) should === (s2.run(c, initial))
    }
  }

  test("ReaderWriterStateT.set is equivalent to modify ignoring first param") {
    forAll { (c: String, initial: Long, s: Long) =>
      val s1 = ReaderWriterStateT.set[Option, String, Long, String](s)
      val s2 = ReaderWriterStateT.modify[Option, String, Long, String](_ => s)

      s1.run(c, initial) should === (s2.run(c, initial))
    }
  }

  test("ReaderWriterStateT.setF is equivalent to modifyF ignoring first param") {
    forAll { (c: String, initial: Long, s: Option[Long]) =>
      val s1 = ReaderWriterStateT.setF[Option, String, Long, String](s)
      val s2 = ReaderWriterStateT.modifyF[Option, String, Long, String](_ => s)

      s1.run(c, initial) should === (s2.run(c, initial))
    }
  }

  test(".get and then .run produces the same state as value") {
    forAll { (c: String, initial: Long, rws: ReaderWriterState[String, Long, String, Long]) =>
      val (_, state, value) = rws.get.run(c, initial).value

      state should === (value)
    }
  }

  test(".get and .flatMap with .get are equivalent") {
    forAll { (c: String, initial: Long, rws: ReaderWriterState[String, Long, String, Long]) =>
      rws.get.run(c, initial) should === (rws.flatMap(_ => ReaderWriterState.get).run(c, initial))
    }
  }

  implicit val iso = CartesianTests.Isomorphisms
    .invariant[ReaderWriterStateT[ListWrapper, String, Int, String, ?]](ReaderWriterStateT.catsDataFunctorForRWST(ListWrapper.functor))

  {
    implicit val F: Monad[ListWrapper] = ListWrapper.monad

    checkAll("ReaderWriterStateT[ListWrapper, String, Int, String, Int]",
      FunctorTests[ReaderWriterStateT[ListWrapper, String, Int, String, ?]].functor[Int, Int, Int])
    checkAll("Functor[ReaderWriterStateT[ListWrapper, String, Int, String, ?]]",
      SerializableTests.serializable(Functor[ReaderWriterStateT[ListWrapper, String, Int, String, ?]]))

    checkAll("ReaderWriterStateT[ListWrapper, String, Int, String, Int]",
      ContravariantTests[ReaderWriterStateT[ListWrapper, ?, Int, String, Int]].contravariant[String, String, String])
    checkAll("Contravariant[ReaderWriterStateT[ListWrapper, ?, Int, String, Int]]",
      SerializableTests.serializable(Contravariant[ReaderWriterStateT[ListWrapper, ?, Int, String, Int]]))

    checkAll("ReaderWriterStateT[ListWrapper, Int, Int, String, Int]",
      ProfunctorTests[ReaderWriterStateT[ListWrapper, ?, Int, String, ?]].profunctor[Int, Int, Int, Int, Int, Int])
    checkAll("Profunctor[ReaderWriterStateT[ListWrapper, ?, Int, String, ?]]",
      SerializableTests.serializable(Profunctor[ReaderWriterStateT[ListWrapper, ?, Int, String, ?]]))

    checkAll("ReaderWriterStateT[ListWrapper, Int, Int, Int, Int]",
      BifunctorTests[ReaderWriterStateT[ListWrapper, String, Int, ?, ?]].bifunctor[Int, Int, Int, Int, Int, Int])
    checkAll("Bifunctor[ReaderWriterStateT[ListWrapper, String, Int, ?, ?]]",
      SerializableTests.serializable(Bifunctor[ReaderWriterStateT[ListWrapper, String, Int, ?, ?]]))
  }

  {
    implicit val LWM: Monad[ListWrapper] = ListWrapper.monad

    checkAll("ReaderWriterStateT[ListWrapper, String, Int, String, Int]",
      MonadTests[ReaderWriterStateT[ListWrapper, String, Int, String, ?]].monad[Int, Int, Int])
    checkAll("Monad[ReaderWriterStateT[ListWrapper, String, Int, String, ?]]",
      SerializableTests.serializable(Monad[ReaderWriterStateT[ListWrapper, String, Int, String, ?]]))
  }

  {
    implicit val LWM: Monad[ListWrapper] = ListWrapper.monad

    checkAll("ReaderWriterStateT[ListWrapper, String, Int, String, Int]",
      MonadStateTests[ReaderWriterStateT[ListWrapper, String, Int, String, ?], Int].monadState[Int, Int, Int])
    checkAll("MonadState[ReaderWriterStateT[ListWrapper, String, Int, String, ?]. Int]",
      SerializableTests.serializable(MonadState[ReaderWriterStateT[ListWrapper, String, Int, String, ?], Int]))
  }

  {
    implicit val LWM: MonadCombine[ListWrapper] = ListWrapper.monadCombine

    checkAll("ReaderWriterStateT[ListWrapper, String, Int, String, Int]",
      MonadCombineTests[ReaderWriterStateT[ListWrapper, String, Int, String, ?]].monadCombine[Int, Int, Int])
    checkAll("MonadCombine[ReaderWriterStateT[ListWrapper, String, Int, String, ?]]",
      SerializableTests.serializable(MonadCombine[ReaderWriterStateT[ListWrapper, String, Int, String, ?]]))
  }

  {
    implicit val LWM: Monad[ListWrapper] = ListWrapper.monad

    checkAll("ReaderWriterStateT[ListWrapper, String, Int, String, Int]",
      MonadReaderTests[ReaderWriterStateT[ListWrapper, String, Int, String, ?], String].monadReader[String, String, String])

    // check serializable using Option
    checkAll("MonadReader[ReaderWriterStateT[Option, String, Int, String, ?], String]",
      SerializableTests.serializable(MonadReader[ReaderWriterStateT[Option, String, Int, String, ?], String]))
  }

  {
    implicit val LWM: Monad[ListWrapper] = ListWrapper.monad

    checkAll("ReaderWriterStateT[ListWrapper, String, Int, String, Int]",
      MonadWriterTests[ReaderWriterStateT[ListWrapper, String, Int, String, ?], String].monadWriter[String, String, String])
    checkAll("MonadWriter[ReaderWriterStateT[ListWrapper, String, Int, String, ?], String]",
      SerializableTests.serializable(MonadWriter[ReaderWriterStateT[ListWrapper, String, Int, String, ?], String]))
  }

  {
    implicit val iso = CartesianTests.Isomorphisms.invariant[ReaderWriterStateT[Option, String, Int, String, ?]]
    implicit val eqEitherTFA: Eq[EitherT[ReaderWriterStateT[Option, String, Int, String, ?], Unit, Int]] =
      EitherT.catsDataEqForEitherT[ReaderWriterStateT[Option, String, Int, String, ?], Unit, Int]

    checkAll("ReaderWriterStateT[Option, String, Int, String, Int]",
      MonadErrorTests[ReaderWriterStateT[Option, String, Int, String, ?], Unit].monadError[Int, Int, Int])
    checkAll("MonadError[ReaderWriterStateT[Option, String, Int, String, ?], Unit]",
      SerializableTests.serializable(MonadError[ReaderWriterStateT[Option, String, Int, String, ?], Unit]))
  }

  {
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    implicit val S: SemigroupK[ListWrapper] = ListWrapper.semigroupK

    checkAll("ReaderWriterStateT[ListWrapper, String, Int, String, Int]",
      SemigroupKTests[ReaderWriterStateT[ListWrapper, String, Int, String, ?]].semigroupK[Int])
    checkAll("SemigroupK[ReaderWriterStateT[ListWrapper, String, Int, String, ?]]",
      SerializableTests.serializable(SemigroupK[ReaderWriterStateT[ListWrapper, String, Int, String, ?]]))
  }

  {
    implicit val F: Monad[ListWrapper] = ListWrapper.monad

    checkAll("ReaderWriterStateT[ListWrapper, String, Int, String, Int]]",
      MonadTransTests[ReaderWriterStateT[?[_], String, Int, String, ?]].monadTrans[ListWrapper, Int, Int])
    checkAll("MonadTrans[ReaderWriterStateT[?[_], String, Int, String, ?]]",
      SerializableTests.serializable(MonadTrans[ReaderWriterStateT[?[_], String, Int, String, ?]]))
  }

}

object ReaderWriterStateTTests {
  def addAndLog(i: Int): ReaderWriterState[String, Int, Vector[String], Int] = {
    import cats.instances.vector._

    ReaderWriterState { (context, state) =>
      (Vector(s"${context}: Added ${i}"), state + i, state + i)
    }
  }

  def addLogUnit(i: Int): ReaderWriterState[String, Int, Unit, Int] = {
    import cats.kernel.instances.unit._

    ReaderWriterState { (context, state) => ((), state + i, state + i) }
  }

  implicit def RWSTEq[F[_], E, S, L, A](implicit S: Arbitrary[S], E: Arbitrary[E], FLSA: Eq[F[(L, S, A)]],
    F: Monad[F]): Eq[ReaderWriterStateT[F, E, S, L, A]] =
    Eq.by[ReaderWriterStateT[F, E, S, L, A], (E, S) => F[(L, S, A)]] { state =>
      (e, s) => state.run(e, s)
    }
}
