package cats
package tests

import scala.math.min

// TODO: monad laws

class EvalTests extends CatsSuite {

  /**
   * Class for spooky side-effects and action-at-a-distance.
   *
   * It is basically a mutable counter that can be used to measure how
   * many times an otherwise pure function is being evaluted.
   */
  class Spooky(var counter: Int = 0)

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
        assert(result === value)
        spin ^= result.##
      }
      assert(spooky.counter == numEvals)
    }
    (0 to 2).foreach(n => nTimes(n, numCalls(n)))
  }

  // has the semantics of lazy val: 0 or 1 evaluations
  def memoized[A](value: A): (Spooky, Eval[A]) = {
    val spooky = new Spooky
    (spooky, Eval.later { spooky.counter += 1; value })
  }

  test("memoized: Eval.later(_)") {
    runValue(999)(memoized)(n => min(n, 1))
  }

  // has the semantics of val: 1 evaluation
  def eager[A](value: A): (Spooky, Eval[A]) = {
    val spooky = new Spooky
    (spooky, Eval.now { spooky.counter += 1; value })
  }

  test("eager: Eval.now(_)") {
    runValue(999)(eager)(n => 1)
  }

  // has the semantics of def: N evaluations
  def always[A](value: A): (Spooky, Eval[A]) = {
    val spooky = new Spooky
    (spooky, Eval.always { spooky.counter += 1; value })
  }

  test("by-name: Eval.always(_)") {
    runValue(999)(always)(n => n)
  }
}
