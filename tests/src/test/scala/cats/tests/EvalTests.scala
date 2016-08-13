package cats
package tests

import scala.math.min
import cats.laws.ComonadLaws
import cats.laws.discipline.{BimonadTests, CartesianTests, MonadTests, SerializableTests}
import cats.laws.discipline.arbitrary._
import cats.kernel.laws.{GroupLaws, OrderLaws}

class EvalTests extends CatsSuite {

  /**
   * Class for spooky side-effects and action-at-a-distance.
   *
   * It is basically a mutable counter that can be used to measure how
   * many times an otherwise pure function is being evaluted.
   */
  class Spooky(var counter: Int = 0) {
    def increment(): Unit = counter += 1
  }

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

  {
    implicit val iso = CartesianTests.Isomorphisms.invariant[Eval]
    checkAll("Eval[Int]", BimonadTests[Eval].bimonad[Int, Int, Int])
    checkAll("Eval[Int]", MonadTests[Eval].monad[Int, Int, Int])
  }
  checkAll("Bimonad[Eval]", SerializableTests.serializable(Bimonad[Eval]))
  checkAll("Monad[Eval]", SerializableTests.serializable(Monad[Eval]))

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
