package cats
package tests

import cats.data.{ ReaderWriterStateT, ReaderWriterState, EitherT }
import cats.functor.{ Contravariant, Profunctor }
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
    forAll { (rwsa: ReaderWriterState[String, Vector[Int], Int, Int], rwsb: ReaderWriterState[String, Vector[Int], Int, Int], c: String, s: Int) =>
      val logMap2 = rwsa.map2(rwsb)((_, _) => ()).runL(c, s).value

      val (logA, stateA, _) = rwsa.run(c, s).value
      val logB = rwsb.runL(c, stateA).value
      val combinedLog = logA |+| logB

      logMap2 should === (combinedLog)
    }
  }

  test("ReaderWriterState.ask provides the context") {
    forAll { (context: String, initial: Int) =>
      ReaderWriterState.ask[String, String, Int].runA(context, initial).value should === (context)
    }
  }

  test("local is consistent with contramap") {
    forAll { (context: Int, initial: Int, f: Int => String) =>
      val rwsa = ReaderWriterState.pure[String, Unit, Int, Unit](()).contramap(f).flatMap(_ => ReaderWriterState.ask)
      val rwsb = ReaderWriterState.pure[String, Unit, Int, Unit](()).local(f).flatMap(_ => ReaderWriterState.ask)

      rwsa.runA(context, initial) should === (rwsb.runA(context, initial))
    }
  }

  test("ReaderWriterState.pure and ReaderWriterStateT.pure are consistent") {
    forAll { (value: Int) =>
      val rws: ReaderWriterState[String, Vector[String], Int, Int] = ReaderWriterState.pure(value)
      val rwst: ReaderWriterState[String, Vector[String], Int, Int] = ReaderWriterStateT.pure(value)

      rws should === (rwst)
    }
  }

  test("ReaderWriterState.pure creates an ReaderWriterState with an empty log") {
    forAll { (context: String, initial: Int) =>
      val rws: ReaderWriterState[String, String, Int, Unit] = ReaderWriterState.pure(())
      rws.run(context, initial).value should === ((Monoid[String].empty, initial, ()))
    }
  }

  test("ReaderWriterState.get and ReaderWriterStateT.get are consistent") {
    forAll { (initial: Int) =>
      val rws: ReaderWriterState[String, Vector[String], Int, Int] = ReaderWriterState.get
      val rwst: ReaderWriterState[String, Vector[String], Int, Int] = ReaderWriterStateT.get

      rws should === (rwst)
    }
  }

  test("ReaderWriterState.get and instance get are consistent") {
    forAll { (initial: Int) =>
      val singleton = ReaderWriterState.inspect[String, String, Int, String](_.toString)
      val instance = ReaderWriterState.pure[String, String, Int, Unit](()).inspect(_.toString)

      singleton should === (instance)
    }
  }

  test("ReaderWriterState.inspect and ReaderWriterStateT.inspect are consistent") {
    forAll { (f: Int => Int) =>
      val rws: ReaderWriterState[String, Vector[String], Int, Int] = ReaderWriterState.inspect(f)
      val rwst: ReaderWriterState[String, Vector[String], Int, Int] = ReaderWriterStateT.inspect(f)

      rws should === (rwst)
    }
  }

  test("ReaderWriterState.inspect and ReaderWriterStateT.inspectF are consistent") {
    forAll { (f: Int => Int) =>
      val rws: ReaderWriterState[String, String, Int, Int] = ReaderWriterState.inspect(f)
      val rwst: ReaderWriterState[String, String, Int, Int] = ReaderWriterStateT.inspectF(f.andThen(Eval.now))

      rws should === (rwst)
    }
  }

  test("ReaderWriterState.modify and ReaderWriterStateT.modify are consistent") {
    forAll { (f: Int => Int) =>
      val rws: ReaderWriterState[String, Vector[String], Int, Unit] = ReaderWriterState.modify(f)
      val rwst: ReaderWriterState[String, Vector[String], Int, Unit] = ReaderWriterStateT.modify(f)

      rws should === (rwst)
    }
  }

  test("ReaderWriterState.modify and ReaderWriterStateT.modifyF are consistent") {
    forAll { (f: Int => Int) =>
      val rws: ReaderWriterState[String, Vector[String], Int, Unit] = ReaderWriterState.modify(f)
      val rwst: ReaderWriterState[String, Vector[String], Int, Unit] = ReaderWriterStateT.modifyF(f.andThen(Eval.now))

      rws should === (rwst)
    }
  }

  test("ReaderWriterState.pure and ReaderWriterStateT.lift are consistent") {
    forAll { (value: Int) =>
      val rws: ReaderWriterState[String, Vector[String], Int, Int] = ReaderWriterState.pure(value)
      val rwst: ReaderWriterState[String, Vector[String], Int, Int] = ReaderWriterStateT.lift(Eval.now(value))

      rws should === (rwst)
    }
  }

  test("ReaderWriterState.set and ReaderWriterStateT.set are consistent") {
    forAll { (next: Int) =>
      val rws: ReaderWriterState[String, Vector[String], Int, Unit] = ReaderWriterState.set(next)
      val rwst: ReaderWriterState[String, Vector[String], Int, Unit] = ReaderWriterStateT.set(next)

      rws should === (rwst)
    }
  }

  test("ReaderWriterState.set and ReaderWriterStateT.setF are consistent") {
    forAll { (next: Int) =>
      val rws: ReaderWriterState[String, Vector[String], Int, Unit] = ReaderWriterState.set(next)
      val rwst: ReaderWriterState[String, Vector[String], Int, Unit] = ReaderWriterStateT.setF(Eval.now(next))

      rws should === (rwst)
    }
  }

  test("ReaderWriterState.tell and ReaderWriterStateT.tell are consistent") {
    forAll { (log: String) =>
      val rws: ReaderWriterState[String, String, Int, Unit] = ReaderWriterState.tell(log)
      val rwst: ReaderWriterState[String, String, Int, Unit] = ReaderWriterStateT.tell(log)

      rws should === (rwst)
    }
  }

  test("ReaderWriterState.tell and ReaderWriterStateT.tellF are consistent") {
    forAll { (log: String) =>
      val rws: ReaderWriterState[String, String, Int, Unit] = ReaderWriterState.tell(log)
      val rwst: ReaderWriterState[String, String, Int, Unit] = ReaderWriterStateT.tellF(Eval.now(log))

      rws should === (rwst)
    }
  }

  test("ReaderWriterState.tell + written is identity") {
    forAll { (context: String, initial: Int, log: String) =>
      ReaderWriterState.tell[String, String, Int](log).written.runA(context, initial).value should === (log)
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
      val pure = ReaderWriterState.pure[String, String, Int, Int](a)
      pure.reset should === (pure)
    }
  }

  test("modify identity is a noop") {
    forAll { (f: ReaderWriterStateT[Option, String, String, String, Int], c: String, initial: String) =>
      f.modify(identity).run(c, initial) should === (f.run(c, initial))
    }
  }

  test("modify modifies only the state") {
    forAll { (rws: ReaderWriterStateT[Option, String, String, Long, Long], c: String, f: Long => Long, initial: Long) =>
      rws.modify(f).runS(c, initial) should === (rws.runS(c, initial).map(f))
      rws.modify(f).runA(c, initial) should === (rws.runA(c, initial))
    }
  }

  test("reset modifies only the log") {
    forAll { (rws: ReaderWriterState[String, String, Int, Int], c: String, s: Int) =>
      rws.reset.runA(c, s) should === (rws.runA(c, s))
      rws.reset.runS(c, s) should === (rws.runS(c, s))
    }
  }

  test("modify is equivalent to get and set") {
    forAll { (c: String, f: Long => Long, initial: Long) =>
      val s1 = ReaderWriterStateT.modify[Option, String, String, Long](f)
      val s2 = for {
        l <- ReaderWriterStateT.get[Option, String, String, Long]
        _ <- ReaderWriterStateT.set[Option, String, String, Long](f(l))
      } yield ()

      s1.run(c, initial) should === (s2.run(c, initial))
    }
  }

  test("ReaderWriterStateT.set is equivalent to modify ignoring first param") {
    forAll { (c: String, initial: Long, s: Long) =>
      val s1 = ReaderWriterStateT.set[Option, String, String, Long](s)
      val s2 = ReaderWriterStateT.modify[Option, String, String, Long](_ => s)

      s1.run(c, initial) should === (s2.run(c, initial))
    }
  }

  test("ReaderWriterStateT.setF is equivalent to modifyF ignoring first param") {
    forAll { (c: String, initial: Long, s: Option[Long]) =>
      val s1 = ReaderWriterStateT.setF[Option, String, String, Long](s)
      val s2 = ReaderWriterStateT.modifyF[Option, String, String, Long](_ => s)

      s1.run(c, initial) should === (s2.run(c, initial))
    }
  }

  test(".get and then .run produces the same state as value") {
    forAll { (c: String, initial: Long, rws: ReaderWriterState[String, String, Long, Long]) =>
      val (_, state, value) = rws.get.run(c, initial).value

      state should === (value)
    }
  }

  test(".get and .flatMap with .get are equivalent") {
    forAll { (c: String, initial: Long, rws: ReaderWriterState[String, String, Long, Long]) =>
      rws.get.run(c, initial) should === (rws.flatMap(_ => ReaderWriterState.get).run(c, initial))
    }
  }

  implicit val iso = CartesianTests.Isomorphisms
    .invariant[ReaderWriterStateT[ListWrapper, String, String, Int, ?]](ReaderWriterStateT.catsDataFunctorForRWST(ListWrapper.functor))

  {
    implicit val F: Monad[ListWrapper] = ListWrapper.monad

    checkAll("ReaderWriterStateT[ListWrapper, String, String, Int, Int]",
      FunctorTests[ReaderWriterStateT[ListWrapper, String, String, Int, ?]].functor[Int, Int, Int])
    checkAll("Functor[ReaderWriterStateT[ListWrapper, String, String, Int, ?]]",
      SerializableTests.serializable(Functor[ReaderWriterStateT[ListWrapper, String, String, Int, ?]]))

    checkAll("ReaderWriterStateT[ListWrapper, String, String, Int, Int]",
      ContravariantTests[ReaderWriterStateT[ListWrapper, ?, String, Int, Int]].contravariant[String, String, String])
    checkAll("Contravariant[ReaderWriterStateT[ListWrapper, ?, String, Int, Int]]",
      SerializableTests.serializable(Contravariant[ReaderWriterStateT[ListWrapper, ?, String, Int, Int]]))

    checkAll("ReaderWriterStateT[ListWrapper, Int, String, Int, Int]",
      ProfunctorTests[ReaderWriterStateT[ListWrapper, ?, String, Int, ?]].profunctor[Int, Int, Int, Int, Int, Int])
    checkAll("Profunctor[ReaderWriterStateT[ListWrapper, ?, String, Int, ?]]",
      SerializableTests.serializable(Profunctor[ReaderWriterStateT[ListWrapper, ?, String, Int, ?]]))

  }

  {
    implicit val LWM: Monad[ListWrapper] = ListWrapper.monad

    checkAll("ReaderWriterStateT[ListWrapper, String, String, Int, Int]",
      MonadTests[ReaderWriterStateT[ListWrapper, String, String, Int, ?]].monad[Int, Int, Int])
    checkAll("Monad[ReaderWriterStateT[ListWrapper, String, String, Int, ?]]",
      SerializableTests.serializable(Monad[ReaderWriterStateT[ListWrapper, String, String, Int, ?]]))
  }

  {
    implicit val iso = CartesianTests.Isomorphisms.invariant[ReaderWriterStateT[Option, String, String, Int, ?]]
    implicit val eqEitherTFA: Eq[EitherT[ReaderWriterStateT[Option, String, String, Int, ?], Unit, Int]] =
      EitherT.catsDataEqForEitherT[ReaderWriterStateT[Option, String, String, Int, ?], Unit, Int]

    checkAll("ReaderWriterStateT[Option, String, String, Int, Int]",
      MonadErrorTests[ReaderWriterStateT[Option, String, String, Int, ?], Unit].monadError[Int, Int, Int])
    checkAll("MonadError[ReaderWriterStateT[Option, String, String, Int, ?], Unit]",
      SerializableTests.serializable(MonadError[ReaderWriterStateT[Option, String, String, Int, ?], Unit]))
  }

  {
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    implicit val S: SemigroupK[ListWrapper] = ListWrapper.semigroupK

    checkAll("ReaderWriterStateT[ListWrapper, String, String, Int, Int]",
      SemigroupKTests[ReaderWriterStateT[ListWrapper, String, String, Int, ?]].semigroupK[Int])
    checkAll("SemigroupK[ReaderWriterStateT[ListWrapper, String, String, Int, ?]]",
      SerializableTests.serializable(SemigroupK[ReaderWriterStateT[ListWrapper, String, String, Int, ?]]))
  }

}

object ReaderWriterStateTTests {
  def addAndLog(i: Int): ReaderWriterState[String, Vector[String], Int, Int] = {
    import cats.instances.vector._

    ReaderWriterState { (context, state) =>
      (Vector(s"${context}: Added ${i}"), state + i, state + i)
    }
  }

  def addLogUnit(i: Int): ReaderWriterState[String, Unit, Int, Int] = {
    import cats.kernel.instances.unit._

    ReaderWriterState { (context, state) => ((), state + i, state + i) }
  }

  implicit def RWSTEq[F[_], E, L, S, A](implicit S: Arbitrary[S], E: Arbitrary[E], FLSA: Eq[F[(L, S, A)]],
    F: Monad[F]): Eq[ReaderWriterStateT[F, E, L, S, A]] =
    Eq.by[ReaderWriterStateT[F, E, L, S, A], (E, S) => F[(L, S, A)]] { state =>
      (e, s) => state.run(e, s)
    }
}
