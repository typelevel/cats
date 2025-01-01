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

import cats.{Contravariant, ContravariantMonoidal, Monad, Semigroupal}
import cats.arrow.{ArrowChoice, Choice, CommutativeArrow}
import cats.data.AndThen
import cats.kernel.laws.discipline.SerializableTests
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import cats.platform.Platform
import munit.ScalaCheckSuite
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Prop.*

class AndThenSuite extends CatsSuite with ScalaCheckSuite {
  checkAll("AndThen[MiniInt, Int]", SemigroupalTests[AndThen[MiniInt, *]].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[AndThen[Int, *]]", SerializableTests.serializable(Semigroupal[AndThen[Int, *]]))

  {
    implicit val iso: SemigroupalTests.Isomorphisms[AndThen[*, Int]] =
      SemigroupalTests.Isomorphisms.invariant[AndThen[*, Int]]
    checkAll("AndThen[*, Int]",
             ContravariantMonoidalTests[AndThen[*, Int]].contravariantMonoidal[MiniInt, Boolean, Boolean]
    )
    checkAll("ContravariantMonoidal[AndThen[*, Int]]",
             SerializableTests.serializable(ContravariantMonoidal[AndThen[*, Int]])
    )
  }

  checkAll("AndThen[MiniInt, Int]", MonadTests[AndThen[MiniInt, *]].monad[Int, Int, Int])
  checkAll("Monad[AndThen[Int, *]]", SerializableTests.serializable(Monad[AndThen[Int, *]]))

  checkAll("AndThen",
           CommutativeArrowTests[AndThen].commutativeArrow[MiniInt, Boolean, Boolean, Boolean, Boolean, Boolean]
  )
  checkAll("Arrow[AndThen]", SerializableTests.serializable(CommutativeArrow[AndThen]))

  checkAll("AndThen", ChoiceTests[AndThen].choice[MiniInt, Boolean, Int, Int])
  checkAll("Choice[AndThen]", SerializableTests.serializable(Choice[AndThen]))

  checkAll("AndThen", ArrowChoiceTests[AndThen].arrowChoice[MiniInt, Boolean, Boolean, Boolean, Boolean, Boolean])
  checkAll("ArrowChoice[AndThen]", SerializableTests.serializable(ArrowChoice[AndThen]))

  checkAll("AndThen[*, Int]", ContravariantTests[AndThen[*, Int]].contravariant[MiniInt, Int, Boolean])
  checkAll("Contravariant[AndThen[*, Int]]", SerializableTests.serializable(Contravariant[AndThen[*, Int]]))

  property("compose a chain of functions with andThen") {
    forAll { (i: Int, fs: List[Int => Int]) =>
      val result = fs.map(AndThen(_)).reduceOption(_.andThen(_)).map(_(i))
      val expect = fs.reduceOption(_.andThen(_)).map(_(i))

      result == expect
    }
  }

  property("compose a chain of functions with compose") {
    forAll { (i: Int, fs: List[Int => Int]) =>
      val result = fs.map(AndThen(_)).reduceOption(_.compose(_)).map(_(i))
      val expect = fs.reduceOption(_.compose(_)).map(_(i))

      result == expect
    }
  }

  test("andThen is stack safe") {
    val count = if (Platform.isJvm) 500000 else 1000
    val fs = (0 until count).map(_ => (i: Int) => i + 1)
    val result = fs.foldLeft(AndThen((x: Int) => x))(_.andThen(_))(42)

    assertEquals(result, count + 42)
  }

  test("compose is stack safe") {
    val count = if (Platform.isJvm) 500000 else 1000
    val fs = (0 until count).map(_ => (i: Int) => i + 1)
    val result = fs.foldLeft(AndThen((x: Int) => x))(_.compose(_))(42)

    assertEquals(result, count + 42)
  }

  test("Function1 andThen is stack safe") {
    val count = if (Platform.isJvm) 50000 else 1000
    val start: (Int => Int) = AndThen((x: Int) => x)
    val fs = (0 until count).foldLeft(start) { (acc, _) =>
      acc.andThen(_ + 1)
    }
    assertEquals(fs(0), count)
  }

  test("toString") {
    assert(AndThen((x: Int) => x).toString.startsWith("AndThen$"))
  }

  // generate a general AndThen which may not be right associated
  def genAndThen[A: Cogen: Arbitrary]: Gen[AndThen[A, A]] = {
    val gfn = Gen.function1[A, A](Arbitrary.arbitrary[A])
    // if we don't have a long list we don't see any Concat
    Gen
      .choose(128, 1 << 13)
      .flatMap { size =>
        Gen.listOfN(size, gfn).flatMap { fns =>
          val ary = fns.toArray

          def loop(start: Int, end: Int): Gen[AndThen[A, A]] =
            if (start == (end - 1)) Gen.const(AndThen(ary(start)))
            else if (start >= end) Gen.const(AndThen(identity[A]))
            else {
              Gen.choose(start, end - 1).flatMap { middle =>
                for {
                  left <- loop(start, middle)
                  right <- loop(middle, end)
                } yield left.andThen(right)
              }
            }

          loop(0, ary.length)
        }
      }
  }

  // generate a right associated function by construction
  def genRight[A: Cogen: Arbitrary]: Gen[AndThen[A, A]] = {
    val gfn = Gen.function1[A, A](Arbitrary.arbitrary[A])
    // if we don't have a long list we don't see any Concat
    Gen
      .choose(128, 1 << 13)
      .flatMap { size =>
        Gen.listOfN(size, gfn).map {
          case Nil => AndThen(identity[A])
          case h :: tail =>
            tail.foldRight(AndThen(h)) { (fn, at) => AndThen(fn).andThen(at) }
        }
      }
  }

  // generate a left associated function by construction
  def genLeft[A: Cogen: Arbitrary]: Gen[AndThen[A, A]] = {
    val gfn = Gen.function1[A, A](Arbitrary.arbitrary[A])
    // if we don't have a long list we don't see any Concat
    Gen
      .choose(1024, 1 << 13)
      .flatMap { size =>
        Gen.listOfN(size, gfn).map {
          case Nil => AndThen(identity[A])
          case h :: tail =>
            tail.foldLeft(AndThen(h)) { (at, fn) => at.andThen(fn) }
        }
      }
  }

  property("toRightAssociated works") {
    // we pass explicit Gens here rather than use the Arbitrary
    // instance which just wraps a function

    // Right associated should be identity
    forAll(genRight[Int]) { at =>
      AndThen.isRightAssociated(AndThen.toRightAssociated(at))
    } &&
    // Left associated is never right associated
    forAll(genLeft[Int]) { at =>
      val notInit = AndThen.isRightAssociated(at)
      val done = AndThen.isRightAssociated(AndThen.toRightAssociated(at))
      !notInit && done
    } &&
    // check that right associating doesn't change the function value
    forAll(genAndThen[Int], Gen.choose(Int.MinValue, Int.MaxValue)) { (at, i) =>
      AndThen.toRightAssociated(at)(i) == at(i)
    } &&
    // in the worst case of a left associated AndThen, values should still match
    forAll(genLeft[Int], Gen.choose(Int.MinValue, Int.MaxValue)) { (at, i) =>
      AndThen.toRightAssociated(at)(i) == at(i)
    }
  }

}
