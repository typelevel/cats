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

import cats.{Bimonad, CommutativeMonad, Eval, Reducible}
import cats.laws.ComonadLaws
import cats.laws.discipline.{
  BimonadTests,
  CommutativeMonadTests,
  DeferTests,
  ReducibleTests,
  SemigroupalTests,
  SerializableTests
}
import cats.laws.discipline.arbitrary.*
import cats.kernel.{Eq, Monoid, Order, PartialOrder, Semigroup}
import cats.kernel.laws.discipline.{EqTests, GroupTests, MonoidTests, OrderTests, PartialOrderTests, SemigroupTests}
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Arbitrary.arbitrary
import scala.annotation.tailrec
import scala.math.min
import cats.syntax.eq.*
import org.scalacheck.Prop.*

class EvalSuite extends CatsSuite {
  implicit val eqThrow: Eq[Throwable] = Eq.allEqual

  /**
   * This method creates a Eval[A] instance (along with a
   * corresponding Spooky instance) from an initial `value` using the
   * given `init` function.
   *
   * It will then proceed to call `value` 0-or-more times, verifying
   * that the result is equal to `value`, and also that the
   * appropriate number of evaluations are occurring using the
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
      assert(spooky.counter === numEvals)
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

  test(".value should evaluate only once on the result of .memoize") {
    val spooky = new Spooky
    val i2 = Eval.always(spooky.increment()).memoize
    val i3 = Eval.now(()).flatMap(_ => Eval.later(spooky.increment())).memoize
    i2.value
    assert(spooky.counter === 1)
    i2.value
    assert(spooky.counter === 1)
    i3.value
    assert(spooky.counter === 2)
    i3.value
    assert(spooky.counter === 2)
  }

  test("Defer and FlatMap compose without blowing the stack") {
    def inc(a: Eval[Int], count: Int): Eval[Int] =
      if (count <= 0) a
      else Eval.defer(Eval.defer(inc(a, count - 1))).flatMap { i => Eval.now(i + 1) }

    assert(inc(Eval.now(0), 1000000).value == 1000000)
  }

  {
    implicit val iso: SemigroupalTests.Isomorphisms[Eval] =
      SemigroupalTests.Isomorphisms.invariant[Eval]
    checkAll("Eval[Int]", BimonadTests[Eval].bimonad[Int, Int, Int])
  }

  checkAll("Eval[Int]", DeferTests[Eval].defer[Int])
  checkAll("Eval[Int]", CommutativeMonadTests[Eval].commutativeMonad[Int, Int, Int])
  checkAll("CommutativeMonad[Eval]", SerializableTests.serializable(CommutativeMonad[Eval]))

  checkAll("Bimonad[Eval]", SerializableTests.serializable(Bimonad[Eval]))

  checkAll("Eval[Int]", ReducibleTests[Eval].reducible[Option, Int, Int])
  checkAll("Reducible[Eval]", SerializableTests.serializable(Reducible[Eval]))

  checkAll("Eval[Int]", GroupTests[Eval[Int]].group)

  {
    implicit val A: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]
    checkAll("Eval[ListWrapper[Int]]", MonoidTests[Eval[ListWrapper[Int]]].monoid)
  }

  {
    implicit val A: Semigroup[ListWrapper[Int]] = ListWrapper.semigroup[Int]
    checkAll("Eval[ListWrapper[Int]]", SemigroupTests[Eval[ListWrapper[Int]]].semigroup)
  }

  {
    implicit val A: Order[ListWrapper[Int]] = ListWrapper.order[Int]
    checkAll("Eval[ListWrapper[Int]]", OrderTests[Eval[ListWrapper[Int]]].order)
  }

  {
    implicit val A: PartialOrder[ListWrapper[Int]] = ListWrapper.partialOrder[Int]
    checkAll("Eval[ListWrapper[Int]]", PartialOrderTests[Eval[ListWrapper[Int]]].partialOrder)
  }

  {
    implicit val A: Eq[ListWrapper[Int]] = ListWrapper.eqv[Int]
    checkAll("Eval[ListWrapper[Int]]", EqTests[Eval[ListWrapper[Int]]].eqv)
  }

  // The following tests check laws which are a different formulation of
  // laws that are checked. Since these laws are more or less duplicates of
  // existing laws, we don't check them for all types that have the relevant
  // instances.

  test("cokleisli left identity") {
    forAll { (fa: Eval[Int], f: Eval[Int] => Long) =>
      val isEq = ComonadLaws[Eval].cokleisliLeftIdentity(fa, f)
      assert(isEq.lhs === (isEq.rhs))
    }
  }

  test("cokleisli right identity") {
    forAll { (fa: Eval[Int], f: Eval[Int] => Long) =>
      val isEq = ComonadLaws[Eval].cokleisliRightIdentity(fa, f)
      assert(isEq.lhs === (isEq.rhs))
    }
  }

  // the following machinery is all to facilitate testing deeply-nested
  // eval values for stack safety. the idea is that we want to
  // randomly generate deep chains of eval operations.
  //
  // there are three ways to construct Eval[A] values from expressions
  // returning A (and which are generated by Arbitrary[Eval[A]]):
  //
  //   - Eval.now(...)
  //   - Eval.later(...)
  //   - Eval.always(...)
  //
  // there are four operations that transform expressions returning
  // Eval[A] into a new Eval[A] value:
  //
  //   - (...).map(f)
  //   - (...).flatMap(g)
  //   - (...).memoize
  //   - Eval.defer(...)
  //
  // the O[A] ast represents these four operations. we generate a very
  // long Vector[O[A]] and a starting () => Eval[A] expression (which
  // we call a "leaf") and then compose these to produce one
  // (deeply-nested) Eval[A] value, which we wrap in DeepEval(_).

  case class DeepEval[A](eval: Eval[A])

  object DeepEval {

    sealed abstract class O[A]

    case class OMap[A](f: A => A) extends O[A]
    case class OFlatMap[A](f: A => Eval[A]) extends O[A]
    case class OMemoize[A]() extends O[A]
    case class ODefer[A]() extends O[A]

    implicit def arbitraryO[A: Arbitrary: Cogen]: Arbitrary[O[A]] =
      Arbitrary(
        Gen.oneOf(arbitrary[A => A].map(OMap(_)),
                  arbitrary[A => Eval[A]].map(OFlatMap(_)),
                  Gen.const(OMemoize[A]()),
                  Gen.const(ODefer[A]())
        )
      )

    def build[A](leaf: () => Eval[A], os: Vector[O[A]]): DeepEval[A] = {

      def restart(i: Int, leaf: () => Eval[A], cbs: List[Eval[A] => Eval[A]]): Eval[A] =
        step(i, leaf, cbs)

      @tailrec def step(i: Int, leaf: () => Eval[A], cbs: List[Eval[A] => Eval[A]]): Eval[A] =
        if (i >= os.length) cbs.foldLeft(leaf())((e, f) => f(e))
        else
          os(i) match {
            case ODefer()    => Eval.defer(restart(i + 1, leaf, cbs))
            case OMemoize()  => step(i + 1, leaf, ((e: Eval[A]) => e.memoize) :: cbs)
            case OMap(f)     => step(i + 1, leaf, ((e: Eval[A]) => e.map(f)) :: cbs)
            case OFlatMap(f) => step(i + 1, leaf, ((e: Eval[A]) => e.flatMap(f)) :: cbs)
          }

      DeepEval(step(0, leaf, Nil))
    }

    // we keep this low to keep CI happy.
    // for an actual stress test increase to 200K or so.
    val MaxDepth = 100

    implicit def arbitraryDeepEval[A: Arbitrary: Cogen]: Arbitrary[DeepEval[A]] = {
      val gen: Gen[O[A]] = arbitrary[O[A]]
      Arbitrary(for {
        leaf <- arbitrary[() => Eval[A]]
        xs <- Gen.containerOfN[Vector, O[A]](MaxDepth, gen)
      } yield DeepEval.build(leaf, xs))
    }
  }

  // all that work for this one little test.

  test("stack safety stress test") {
    forAll { (d: DeepEval[Int]) =>
      try {
        d.eval.value
        assert(true)
      } catch {
        case (e: StackOverflowError) =>
          fail(s"stack overflowed with eval-depth ${DeepEval.MaxDepth}")
      }
    }
  }

  test("memoize handles branched evaluation correctly") {
    forAll { (e: Eval[Int], fn: Int => Eval[Int]) =>
      var n0 = 0
      val a0 = e.flatMap { i =>
        n0 += 1; fn(i);
      }.memoize
      assert(a0.flatMap(i1 => a0.map(i1 == _)).value == true)
      assert(n0 == 1)

      var n1 = 0
      val a1 = Eval.defer { n1 += 1; fn(0) }.memoize
      assert(a1.flatMap(i1 => a1.map(i1 == _)).value == true)
      assert(n1 == 1)

      var n2 = 0
      val a2 = Eval.defer { n2 += 1; fn(0) }.memoize
      assert(Eval.defer(a2).value == Eval.defer(a2).value)
      assert(n2 == 1)
    }
  }

  test("test Defer.recursiveFn example") {
    val sumTo: Int => Eval[Int] =
      cats.Defer[Eval].recursiveFn[Int, Int] { recur =>

        { i =>
          if (i > 0) recur(i - 1).map(_ + i)
          else Eval.now(0)
        }
      }

    assert(sumTo(300000).value == (0 to 300000).sum)
  }
}
