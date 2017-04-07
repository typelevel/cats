package cats
package tests

import cats.data.{ RWST, RWS, EitherT }
import cats.laws.discipline._
import cats.laws.discipline.eq._
import cats.laws.discipline.arbitrary._
import org.scalacheck.{ Arbitrary }

class RWSTTests extends CatsSuite {
  import RWSTTests._

  test("Basic RWS usage") {
    forAll { (context: String, initial: Int) =>
      val (log, state, result) = addAndLog(5).run(context, initial).value

      log should === (Vector(s"${context}: Added 5"))
      state should === (initial + 5)
      result should === (initial + 5)
    }
  }

  test("Traversing with RWS is stack-safe") {
    val ns = (0 to 100000).toList
    val rws = ns.traverse(_ => addAndLog(1))

    rws.runS("context", 0).value should === (100001)
  }

  test("map2 combines logs") {
    forAll { (rwsa: RWS[String, Int, Vector[Int], Int], rwsb: RWS[String, Int, Vector[Int], Int], c: String, s: Int) =>
      val logMap2 = rwsa.map2(rwsb)((_, _) => ()).runL(c, s).value

      val (logA, stateA, _) = rwsa.run(c, s).value
      val logB = rwsb.runL(c, stateA).value
      val combinedLog = logA |+| logB

      logMap2 should === (combinedLog)
    }
  }

  test("RWS.ask provides the context") {
    forAll { (context: String, initial: Int) =>
      RWS.ask[String, Int, String].runA(context, initial).value should === (context)
    }
  }

  test("local is consistent with contramap") {
    forAll { (context: Int, initial: Int, f: Int => String) =>
      val rwsa = RWS.pure[String, Int, Unit, Unit](()).contramap(f).flatMap(_ => RWS.ask)
      val rwsb = RWS.pure[String, Int, Unit, Unit](()).local(f).flatMap(_ => RWS.ask)

      rwsa.runA(context, initial) should === (rwsb.runA(context, initial))
    }
  }

  test("RWS.pure and RWST.pure are consistent") {
    forAll { (value: Int) =>
      val rws: RWS[String, Int, Vector[String], Int] = RWS.pure(value)
      val rwst: RWS[String, Int, Vector[String], Int] = RWST.pure(value)

      rws should === (rwst)
    }
  }

  test("RWS.pure creates an RWS with an empty log") {
    forAll { (context: String, initial: Int) =>
      val rws: RWS[String, Int, String, Unit] = RWS.pure(())
      rws.run(context, initial).value should === ((Monoid[String].empty, initial, ()))
    }
  }

  test("RWS.get and RWST.get are consistent") {
    forAll { (initial: Int) =>
      val rws: RWS[String, Int, Vector[String], Int] = RWS.get
      val rwst: RWS[String, Int, Vector[String], Int] = RWST.get

      rws should === (rwst)
    }
  }

  test("RWS.get and instance get are consistent") {
    forAll { (initial: Int) =>
      val singleton = RWS.inspect[String, Int, String, String](_.toString)
      val instance = RWS.pure[String, Int, String, Unit](()).inspect(_.toString)

      singleton should === (instance)
    }
  }

  test("RWS.inspect and RWST.inspect are consistent") {
    forAll { (f: Int => Int) =>
      val rws: RWS[String, Int, Vector[String], Int] = RWS.inspect(f)
      val rwst: RWS[String, Int, Vector[String], Int] = RWST.inspect(f)

      rws should === (rwst)
    }
  }

  test("RWS.inspect and RWST.inspectF are consistent") {
    forAll { (f: Int => Int) =>
      val rws: RWS[String, Int, String, Int] = RWS.inspect(f)
      val rwst: RWS[String, Int, String, Int] = RWST.inspectF(f.andThen(Eval.now))

      rws should === (rwst)
    }
  }

  test("RWS.modify and RWST.modify are consistent") {
    forAll { (f: Int => Int) =>
      val rws: RWS[String, Int, Vector[String], Unit] = RWS.modify(f)
      val rwst: RWS[String, Int, Vector[String], Unit] = RWST.modify(f)

      rws should === (rwst)
    }
  }

  test("RWS.modify and RWST.modifyF are consistent") {
    forAll { (f: Int => Int) =>
      val rws: RWS[String, Int, Vector[String], Unit] = RWS.modify(f)
      val rwst: RWS[String, Int, Vector[String], Unit] = RWST.modifyF(f.andThen(Eval.now))

      rws should === (rwst)
    }
  }

  test("RWS.pure and RWST.lift are consistent") {
    forAll { (value: Int) =>
      val rws: RWS[String, Int, Vector[String], Int] = RWS.pure(value)
      val rwst: RWS[String, Int, Vector[String], Int] = RWST.lift(Eval.now(value))

      rws should === (rwst)
    }
  }

  test("RWS.set and RWST.set are consistent") {
    forAll { (next: Int) =>
      val rws: RWS[String, Int, Vector[String], Unit] = RWS.set(next)
      val rwst: RWS[String, Int, Vector[String], Unit] = RWST.set(next)

      rws should === (rwst)
    }
  }

  test("RWS.set and RWST.setF are consistent") {
    forAll { (next: Int) =>
      val rws: RWS[String, Int, Vector[String], Unit] = RWS.set(next)
      val rwst: RWS[String, Int, Vector[String], Unit] = RWST.setF(Eval.now(next))

      rws should === (rwst)
    }
  }

  test("RWS.tell and RWST.tell are consistent") {
    forAll { (log: String) =>
      val rws: RWS[String, Int, String, Unit] = RWS.tell(log)
      val rwst: RWS[String, Int, String, Unit] = RWST.tell(log)

      rws should === (rwst)
    }
  }

  test("RWS.tell and RWST.tellF are consistent") {
    forAll { (log: String) =>
      val rws: RWS[String, Int, String, Unit] = RWS.tell(log)
      val rwst: RWS[String, Int, String, Unit] = RWST.tellF(Eval.now(log))

      rws should === (rwst)
    }
  }

  test("RWS.tell + written is identity") {
    forAll { (context: String, initial: Int, log: String) =>
      RWS.tell[String, Int, String](log).written.runA(context, initial).value should === (log)
    }
  }

  test("Cartesian syntax is usable on RWS") {
    val rws = addAndLog(5) *> addAndLog(10)
    val (log, state, result) = rws.run("context", 0).value

    log should === (Vector("context: Added 5", "context: Added 10"))
    state should === (15)
    result should === (15)
  }

  test("flatMap and flatMapF+tell are consistent") {
    forAll { (rwst: RWST[Option, String, String, String, Int], f: Int => Option[Int], initial: String, context: String, log: String) =>
      val flatMap = rwst.flatMap { a =>
        RWST { (e, s) =>
          f(a).map((log, s, _))
        }
      }

      val flatMapF = rwst.flatMapF(f).tell(log)

      flatMap.run(context, initial) should === (flatMapF.run(context, initial))
    }
  }

  test("runEmpty, runEmptyS, runEmptyA and runEmptyL are consistent") {
    forAll { (f: RWST[Option, String, String, String, Int], c: String) =>
      (f.runEmptyL(c) |@| f.runEmptyS(c) |@| f.runEmptyA(c)).tupled should === (f.runEmpty(c))
    }
  }

  test("reset on pure is a noop") {
    forAll { (c: String, s: Int, a: Int) =>
      val pure = RWS.pure[String, Int, String, Int](a)
      pure.reset should === (pure)
    }
  }

  test("modify identity is a noop") {
    forAll { (f: RWST[Option, String, String, String, Int], c: String, initial: String) =>
      f.modify(identity).run(c, initial) should === (f.run(c, initial))
    }
  }

  test("modify modifies only the state") {
    forAll { (rws: RWST[Option, String, Long, String, Long], c: String, f: Long => Long, initial: Long) =>
      rws.modify(f).runS(c, initial) should === (rws.runS(c, initial).map(f))
      rws.modify(f).runA(c, initial) should === (rws.runA(c, initial))
    }
  }

  test("reset modifies only the log") {
    forAll { (rws: RWS[String, Int, String, Int], c: String, s: Int) =>
      rws.reset.runA(c, s) should === (rws.runA(c, s))
      rws.reset.runS(c, s) should === (rws.runS(c, s))
    }
  }

  test("modify is equivalent to get and set") {
    forAll { (c: String, f: Long => Long, initial: Long) =>
      val s1 = RWST.modify[Option, String, Long, String](f)
      val s2 = for {
        l <- RWST.get[Option, String, Long, String]
        _ <- RWST.set[Option, String, Long, String](f(l))
      } yield ()

      s1.run(c, initial) should === (s2.run(c, initial))
    }
  }

  test("RWST.set is equivalent to modify ignoring first param") {
    forAll { (c: String, initial: Long, s: Long) =>
      val s1 = RWST.set[Option, String, Long, String](s)
      val s2 = RWST.modify[Option, String, Long, String](_ => s)

      s1.run(c, initial) should === (s2.run(c, initial))
    }
  }

  test("RWST.setF is equivalent to modifyF ignoring first param") {
    forAll { (c: String, initial: Long, s: Option[Long]) =>
      val s1 = RWST.setF[Option, String, Long, String](s)
      val s2 = RWST.modifyF[Option, String, Long, String](_ => s)

      s1.run(c, initial) should === (s2.run(c, initial))
    }
  }

  test(".get and then .run produces the same state as value") {
    forAll { (c: String, initial: Long, rws: RWS[String, Long, String, Long]) =>
      val (_, state, value) = rws.get.run(c, initial).value

      state should === (value)
    }
  }

  test(".get and .flatMap with .get are equivalent") {
    forAll { (c: String, initial: Long, rws: RWS[String, Long, String, Long]) =>
      rws.get.run(c, initial) should === (rws.flatMap(_ => RWS.get).run(c, initial))
    }
  }

  implicit val iso = CartesianTests.Isomorphisms.invariant[RWST[ListWrapper, String, Int, String, ?]](RWST.catsDataFunctorForRWST(ListWrapper.monad))

  {
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    checkAll("RWST[ListWrapper, String, Int, String, Int]", FunctorTests[RWST[ListWrapper, String, Int, String, ?]].functor[Int, Int, Int])
    checkAll("RWST[ListWrapper, String, Int, String, Int]", ContravariantTests[RWST[ListWrapper, ?, Int, String, Int]].contravariant[String, String, String])
    checkAll("RWST[ListWrapper, Int, Int, String, Int]", ProfunctorTests[RWST[ListWrapper, ?, Int, String, ?]].profunctor[Int, Int, Int, Int, Int, Int])
    checkAll("RWST[ListWrapper, Int, Int, Int, Int]", BifunctorTests[RWST[ListWrapper, String, Int, ?, ?]].bifunctor[Int, Int, Int, Int, Int, Int])
  }

  {
    implicit val LWM: Monad[ListWrapper] = ListWrapper.monad

    checkAll("RWST[ListWrapper, String, Int, String, Int]", MonadTests[RWST[ListWrapper, String, Int, String, ?]].monad[Int, Int, Int])
  }

  {
    implicit val LWM: Monad[ListWrapper] = ListWrapper.monad

    checkAll("RWST[ListWrapper, String, Int, String, Int]", MonadStateTests[RWST[ListWrapper, String, Int, String, ?], Int].monadState[Int, Int, Int])
  }

  {
    implicit val LWM: MonadCombine[ListWrapper] = ListWrapper.monadCombine

    checkAll("RWST[ListWrapper, String, Int, String, Int]", MonadCombineTests[RWST[ListWrapper, String, Int, String, ?]].monadCombine[Int, Int, Int])
  }

  {
    implicit val LWM: Monad[ListWrapper] = ListWrapper.monad

    checkAll("RWST[ListWrapper, String, Int, String, Int]", MonadReaderTests[RWST[ListWrapper, String, Int, String, ?], String].monadReader[String, String, String])
  }

  {
    implicit val LWM: Monad[ListWrapper] = ListWrapper.monad

    checkAll("RWST[ListWrapper, String, Int, String, Int]", MonadWriterTests[RWST[ListWrapper, String, Int, String, ?], String].monadWriter[String, String, String])
  }

  {
    implicit val iso = CartesianTests.Isomorphisms.invariant[RWST[Option, String, Int, String, ?]]
    implicit val eqEitherTFA: Eq[EitherT[RWST[Option, String, Int, String, ?], Unit, Int]] = EitherT.catsDataEqForEitherT[RWST[Option, String, Int, String, ?], Unit, Int]

    checkAll("RWST[Option, String, Int, String, Int]", MonadErrorTests[RWST[Option, String, Int, String, ?], Unit].monadError[Int, Int, Int])
  }

  {
    implicit def F = ListWrapper.monad
    implicit def S = ListWrapper.semigroupK

    checkAll("RWST[ListWrapper, String, Int, String, Int]", SemigroupKTests[RWST[ListWrapper, String, Int, String, ?]].semigroupK[Int])
  }
}

object RWSTTests {
  def addAndLog(i: Int): RWS[String, Int, Vector[String], Int] = {
    import cats.instances.vector._

    RWS { (context, state) =>
      (Vector(s"${context}: Added ${i}"), state + i, state + i)
    }
  }

  implicit def RWSTArbitrary[F[_]: Applicative, E, S, L, A](implicit F: Arbitrary[(E, S) => F[(L, S, A)]]): Arbitrary[RWST[F, E, S, L, A]] =
    Arbitrary(F.arbitrary.map(RWST(_)))

  implicit def RWSTEq[F[_], E, S, L, A](implicit S: Arbitrary[S], E: Arbitrary[E], FLSA: Eq[F[(L, S, A)]],
    F: Monad[F]): Eq[RWST[F, E, S, L, A]] =
    Eq.by[RWST[F, E, S, L, A], (E, S) => F[(L, S, A)]] { state =>
      (e, s) => state.run(e, s)
    }
}
