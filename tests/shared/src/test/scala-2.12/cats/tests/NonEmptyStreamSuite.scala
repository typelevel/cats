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

import cats.{Comonad, Eval, Functor, Monad, NonEmptyTraverse, Now, Reducible, SemigroupK, Show}
import cats.data.{NonEmptyStream, OneAnd}
import cats.kernel.Semigroup
import cats.kernel.instances.order.catsKernelOrderingForOrder
import cats.kernel.laws.discipline.{EqTests, SemigroupTests}
import cats.laws.discipline.*
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.arbitrary.*
import cats.syntax.foldable.*
import cats.syntax.reducible.*
import cats.syntax.show.*
import cats.syntax.eq.*
import org.scalacheck.Prop.*
import org.scalacheck.Test.Parameters

class NonEmptyStreamSuite extends CatsSuite {
  // Lots of collections here.. telling ScalaCheck to calm down a bit
  implicit override val scalaCheckTestParameters: Parameters =
    Parameters.default.withMinSuccessfulTests(20).withMaxSize(Parameters.default.minSize + 5)

  checkAll("NonEmptyStream[Int]", EqTests[NonEmptyStream[Int]].eqv)

  checkAll("NonEmptyStream[Int] with Option",
           NonEmptyTraverseTests[NonEmptyStream].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
  )
  checkAll("NonEmptyTraverse[NonEmptyStream[A]]", SerializableTests.serializable(NonEmptyTraverse[NonEmptyStream[*]]))

  checkAll("NonEmptyStream[Int]", ReducibleTests[NonEmptyStream].reducible[Option, Int, Int])
  checkAll("Reducible[NonEmptyStream]", SerializableTests.serializable(Reducible[NonEmptyStream]))

  checkAll("NonEmptyStream[Int]", SemigroupTests[NonEmptyStream[Int]].semigroup)
  checkAll("Semigroup[NonEmptyStream[Int]]", SerializableTests.serializable(Semigroup[NonEmptyStream[Int]]))

  checkAll("NonEmptyStream[Int]", ShortCircuitingTests[NonEmptyStream].foldable[Int])
  checkAll("NonEmptyStream[Int]", ShortCircuitingTests[NonEmptyStream].traverse[Int])
  checkAll("NonEmptyStream[Int]", ShortCircuitingTests[NonEmptyStream].nonEmptyTraverse[Int])

  {
    // Test functor and subclasses don't have implicit conflicts
    implicitly[Functor[NonEmptyStream]]
    implicitly[Monad[NonEmptyStream]]
    implicitly[Comonad[NonEmptyStream]]
  }

  implicit val iso2: Isomorphisms[NonEmptyStream] = Isomorphisms.invariant[NonEmptyStream]

  checkAll("NonEmptyStream[Int]", MonadTests[NonEmptyStream].monad[Int, Int, Int])
  checkAll("Monad[NonEmptyStream[A]]", SerializableTests.serializable(Monad[NonEmptyStream]))

  checkAll("NonEmptyStream[Int]", ComonadTests[NonEmptyStream].comonad[Int, Int, Int])
  checkAll("Comonad[NonEmptyStream[A]]", SerializableTests.serializable(Comonad[NonEmptyStream]))

  test("Show is not empty and is formatted as expected") {
    forAll { (nel: NonEmptyStream[Int]) =>
      assert(nel.show.nonEmpty === true)
      assert(nel.show.startsWith("OneAnd(") === true)
      assert(nel.show === (implicitly[Show[NonEmptyStream[Int]]].show(nel)))
      assert(nel.show.contains(nel.head.show) === true)
    }
  }

  test("Show is formatted correctly") {
    val oneAnd = NonEmptyStream("Test")
    assert(oneAnd.show === s"OneAnd(Test, Stream())")
  }

  test("Creating OneAnd + unwrap is identity") {
    forAll { (i: Int, tail: Stream[Int]) =>
      val stream = i #:: tail
      val oneAnd = NonEmptyStream(i, tail: _*)
      assert(stream === (oneAnd.unwrap))
    }
  }

  test("NonEmptyStream#find is consistent with Stream#find") {
    forAll { (nel: NonEmptyStream[Int], p: Int => Boolean) =>
      val stream = nel.unwrap
      assert(nel.find(p) === (stream.find(p)))
    }
  }

  test("NonEmptyStream#exists is consistent with Stream#exists") {
    forAll { (nel: NonEmptyStream[Int], p: Int => Boolean) =>
      val stream = nel.unwrap
      assert(nel.exists(p) === (stream.exists(p)))
    }
  }

  test("NonEmptyStream#forall is consistent with Stream#forall") {
    forAll { (nel: NonEmptyStream[Int], p: Int => Boolean) =>
      val stream = nel.unwrap
      assert(nel.forall(p) === (stream.forall(p)))
    }
  }

  test("NonEmptyStream#map is consistent with Stream#map") {
    forAll { (nel: NonEmptyStream[Int], p: Int => String) =>
      val stream = nel.unwrap
      assert(nel.map(p).unwrap === (stream.map(p)))
    }
  }

  test("NonEmptyStream#nonEmptyPartition remains sorted") {
    forAll { (nes: NonEmptyStream[Int], f: Int => Either[String, String]) =>
      val nesf = nes.map(f)
      val sortedStream = (nesf.head #:: nesf.tail).sorted
      val sortedNes = OneAnd(sortedStream.head, sortedStream.tail)
      val ior = Reducible[NonEmptyStream].nonEmptyPartition(sortedNes)(identity)

      assert(ior.left.forall(xs => xs.sorted === xs))
      assert(ior.right.map(xs => xs.sorted === xs).getOrElse(true))
    }
  }

  test("reduceLeft consistent with foldLeft") {
    forAll { (nel: NonEmptyStream[Int], f: (Int, Int) => Int) =>
      assert(nel.reduceLeft(f) === nel.tail.foldLeft(nel.head)(f))
    }
  }

  test("reduceRight consistent with foldRight") {
    forAll { (nel: NonEmptyStream[Int], f: (Int, Eval[Int]) => Eval[Int]) =>
      val got = nel.reduceRight(f).value
      val last :: rev = nel.unwrap.toList.reverse
      val expected = rev.reverse.foldRight(last)((a, b) => f(a, Now(b)).value)
      assert(got === expected)
    }
  }

  test("reduce consistent with fold") {
    forAll { (nel: NonEmptyStream[Int]) =>
      assert(nel.reduce === (nel.fold))
    }
  }

  test("reduce consistent with reduceK") {
    forAll { (nel: NonEmptyStream[Option[Int]]) =>
      assert(nel.reduce(SemigroupK[Option].algebra[Int]) === (nel.reduceK))
    }
  }

  test("reduceLeftToOption consistent with foldLeft + Option") {
    forAll { (nel: NonEmptyStream[Int], f: Int => String, g: (String, Int) => String) =>
      val expected = nel.tail.foldLeft(Option(f(nel.head))) { (opt, i) =>
        opt.map(s => g(s, i))
      }
      assert(nel.reduceLeftToOption(f)(g) === expected)
    }
  }

  test("reduceRightToOption consistent with foldRight + Option") {
    forAll { (nel: NonEmptyStream[Int], f: Int => String, g: (Int, Eval[String]) => Eval[String]) =>
      val got = nel.reduceRightToOption(f)(g).value
      val last :: rev = nel.unwrap.toList.reverse
      val expected = rev.reverse.foldRight(Option(f(last))) { (i, opt) =>
        opt.map(s => g(i, Now(s)).value)
      }
      assert(got === expected)
    }
  }

  test("filter includes elements based on a predicate") {
    forAll { (nes: NonEmptyStream[Int], pred: Int => Boolean) =>
      assert(nes.filter(pred) === (nes.unwrap.filter(pred)))
    }
  }

}

class ReducibleNonEmptyStreamSuite extends ReducibleSuite[NonEmptyStream]("NonEmptyStream") {
  def iterator[T](nes: NonEmptyStream[T]): Iterator[T] =
    (nes.head #:: nes.tail).iterator

  def range(start: Long, endInclusive: Long): NonEmptyStream[Long] = {
    // if we inline this we get a bewildering implicit numeric widening
    // error message in Scala 2.10
    val tailStart: Long = start + 1L
    NonEmptyStream(start, tailStart.to(endInclusive).toStream)
  }

  def fromValues[A](el: A, els: A*): NonEmptyStream[A] = NonEmptyStream(el, els: _*)
}
