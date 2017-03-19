package cats
package tests

import scala.math.min
import scala.util.Try
import cats.laws.ComonadLaws
import cats.laws.discipline.{BimonadTests, CartesianTests, MonadErrorTests, ReducibleTests, SerializableTests}
import cats.laws.discipline.arbitrary._
import cats.kernel.laws.{GroupLaws, OrderLaws}

class EvalTests extends CatsSuite {
  implicit val eqThrow: Eq[Throwable] = Eq.allEqual

  /**
   * This method creates a Eval[A] instance (along with a
   * corresponding Spooky instance) from an initial `value` using the
   * given `init` function.
   *
   * It will then proceed to call `value` 0-or-more times, verifying
   * that the result is equal to `value`, and also that the
   * appropriate number of evaluations are occuring using the
   * `numCalls` function.
   *
   * In other words, each invocation of run says:
   *
   *  1. What underlying `value` to use.
   *  2. How to create Eval instances (memoized, eager, or by-name).
   *  3. How many times we expect the value to be computed.
   */
  def runValue[A: Eq](value: A)(init: A => (Spooky, Eval[A]))(numCalls: Int => Int): Unit = {
    var spin = 0
    def nTimes(n: Int, numEvals: Int): Unit = {
      val (spooky, lz) = init(value)
      (0 until n).foreach { _ =>
        val result = lz.value
        result should === (value)
        spin ^= result.##
      }
      spooky.counter should === (numEvals)
      ()
    }
    (0 to 2).foreach(n => nTimes(n, numCalls(n)))
  }

  // has the semantics of lazy val: 0 or 1 evaluations
  def memoized[A](value: A): (Spooky, Eval[A]) = {
    val spooky = new Spooky
    (spooky, Eval.later { spooky.increment(); value })
  }

  test("memoized: Eval.later(_)") {
    runValue(999)(memoized)(n => min(n, 1))
  }

  // has the semantics of val: 1 evaluation
  def eager[A](value: A): (Spooky, Eval[A]) = {
    val spooky = new Spooky
    (spooky, Eval.now { spooky.increment(); value })
  }

  test("eager: Eval.now(_)") {
    runValue(999)(eager)(n => 1)
  }

  // has the semantics of def: N evaluations
  def always[A](value: A): (Spooky, Eval[A]) = {
    val spooky = new Spooky
    (spooky, Eval.always { spooky.increment(); value })
  }

  test("by-name: Eval.always(_)") {
    runValue(999)(always)(n => n)
  }

  test(".value should evaluate only once on the result of .memoize"){
    forAll { i: Eval[Int] =>
      val spooky = new Spooky
      val i2 = i.map(_ => spooky.increment).memoize
      i2.value
      spooky.counter should === (1)
      i2.value
      spooky.counter should === (1)
    }
  }

  test("handleErrorWith is semi-stacksafe") {
    def loop(i: Int, e: Eval[Int]): Eval[Int] =
      if (i <= 0) e
      else if (i % 100 == 0) loop(i - 1, e.map(n => n).handleErrorWith(_ => Now(-999)))
      else loop(i - 1, e.map(n => n))

    // we expect to use ~1k stack frames of error handling, which
    // should be safe.
    loop(100000, Now(6)).value should === (6)
  }

  test("Eval.raise(t).handleErrorWith(f) = f(t)") {
    forAll { (t: Throwable, f: Throwable => Eval[Int]) =>
      Eval.raise(t).handleErrorWith(f) should === (f(t))
    }
  }

  test(".recoverWith and .handleErrorWith are equivalent") {
    forAll { (e0: Eval[Int], f: Throwable => Eval[Int], p: PartialFunction[Throwable, Eval[Int]]) =>

      // should be an error at least 1/3 of the time.
      val e = e0.map { n => if (n % 3 == 0) n / 0 else n / 2 }

      // test with a total recovery function
      val x1 = Try(e.handleErrorWith(f))
      val y1 = Try(e.recoverWith { case e => f(e) })
      x1 should === (y1)

      // test with a partial recovery function
      val x2 = Try(e.recoverWith(p).value)
      val y2 = Try(e.handleErrorWith(t => if (p.isDefinedAt(t)) p(t) else Eval.raise(t)).value)
      x2 should === (y2)

      // ensure that this works if we throw directly
      val z2 = Try(e.handleErrorWith(t => if (p.isDefinedAt(t)) p(t) else throw t).value)
      x2 should === (z2)
    }
  }

  {
    implicit val iso = CartesianTests.Isomorphisms.invariant[Eval]
    checkAll("Eval[Int]", BimonadTests[Eval].bimonad[Int, Int, Int])

    {
      // we need exceptions which occur during .value calls to be
      // equal to each other (equivalent behavior).
      implicit def eqWithTry[A: Eq]: Eq[Eval[A]] =
        Eq[Try[A]].on((e: Eval[A]) => Try(e.value))

      checkAll("Eval[Int]", MonadErrorTests[Eval, Throwable].monadError[Int, Int, Int])
    }
  }
  checkAll("Bimonad[Eval]", SerializableTests.serializable(Bimonad[Eval]))
  checkAll("MonadError[Eval, Throwable]", SerializableTests.serializable(MonadError[Eval, Throwable]))

  checkAll("Eval[Int]", ReducibleTests[Eval].reducible[Option, Int, Int])
  checkAll("Reducible[Eval]", SerializableTests.serializable(Reducible[Eval]))

  checkAll("Eval[Int]", GroupLaws[Eval[Int]].group)

  {
    implicit val A = ListWrapper.monoid[Int]
    checkAll("Eval[ListWrapper[Int]]", GroupLaws[Eval[ListWrapper[Int]]].monoid)
  }

  {
    implicit val A = ListWrapper.semigroup[Int]
    checkAll("Eval[ListWrapper[Int]]", GroupLaws[Eval[ListWrapper[Int]]].semigroup)
  }

  {
    implicit val A = ListWrapper.order[Int]
    checkAll("Eval[ListWrapper[Int]]", OrderLaws[Eval[ListWrapper[Int]]].order)
  }

  {
    implicit val A = ListWrapper.partialOrder[Int]
    checkAll("Eval[ListWrapper[Int]]", OrderLaws[Eval[ListWrapper[Int]]].partialOrder)
  }

  {
    implicit val A = ListWrapper.eqv[Int]
    checkAll("Eval[ListWrapper[Int]]", OrderLaws[Eval[ListWrapper[Int]]].eqv)
  }

  // The following tests check laws which are a different formulation of
  // laws that are checked. Since these laws are more or less duplicates of
  // existing laws, we don't check them for all types that have the relevant
  // instances.

  test("cokleisli left identity") {
    forAll { (fa: Eval[Int], f: Eval[Int] => Long) =>
      val isEq = ComonadLaws[Eval].cokleisliLeftIdentity(fa, f)
      isEq.lhs should === (isEq.rhs)
    }
  }

  test("cokleisli right identity") {
    forAll { (fa: Eval[Int], f: Eval[Int] => Long) =>
      val isEq = ComonadLaws[Eval].cokleisliRightIdentity(fa, f)
      isEq.lhs should === (isEq.rhs)
    }
  }
}
