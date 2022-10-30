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

import cats._
import cats.kernel.compat.scalaVersionSpecific._
import cats.syntax.all._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

@suppressUnusedImportWarningForScalaVersionSpecific
abstract class TraverseSuite[F[_]: Traverse](name: String)(implicit ArbFInt: Arbitrary[F[Int]]) extends CatsSuite {

  test(s"Traverse[$name].zipWithIndex") {
    forAll { (fa: F[Int]) =>
      assert(fa.zipWithIndex.toList === (fa.toList.zipWithIndex))
    }
  }

  test(s"Traverse[$name].zipWithLongIndex") {
    forAll { (fa: F[Int]) =>
      assert(fa.zipWithLongIndex.toList === (fa.toList.zipWithLongIndex))
    }
  }

  test(s"Traverse[$name].mapAccumulate") {
    forAll { (init: Int, fa: F[Int], fn: ((Int, Int)) => (Int, Int)) =>
      val lhs = fa.mapAccumulate(init)((s, a) => fn((s, a)))

      val rhs = fa.foldLeft((init, List.empty[Int])) { case ((s1, acc), a) =>
        val (s2, b) = fn((s1, a))
        (s2, b :: acc)
      }

      assert(lhs.map(_.toList) === rhs.map(_.reverse))
    }
  }

  test(s"Traverse[$name].mapWithIndex") {
    forAll { (fa: F[Int], fn: ((Int, Int)) => Int) =>
      assert(fa.mapWithIndex((a, i) => fn((a, i))).toList === (fa.toList.zipWithIndex.map(fn)))
    }
  }

  test(s"Traverse[$name].mapWithLongIndex") {
    forAll { (fa: F[Int], fn: ((Int, Long)) => Int) =>
      assert(fa.mapWithLongIndex((a, i) => fn((a, i))).toList === (fa.toList.zipWithLongIndex.map(fn)))
    }
  }

  test(s"Traverse[$name].traverseWithIndexM") {
    forAll { (fa: F[Int], fn: ((Int, Int)) => (Int, Int)) =>
      val left = fa.traverseWithIndexM((a, i) => fn((a, i))).fmap(_.toList)
      val (xs, values) = fa.toList.zipWithIndex.map(fn).unzip
      assert(left === ((xs.combineAll, values)))
    }
  }

  test(s"Traverse[$name].traverseWithLongIndexM") {
    forAll { (fa: F[Int], fn: ((Int, Long)) => (Int, Long)) =>
      val left = fa.traverseWithLongIndexM((a, i) => fn((a, i))).fmap(_.toList)
      val (xs, values) = fa.toList.zipWithLongIndex.map(fn).unzip
      assert(left === ((xs.combineAll, values)))
    }
  }

  test(s"Traverse[$name].traverse matches traverse_ with Option") {
    forAll { (fa: F[Int], fn: Int => Option[Int]) =>
      assert(Applicative[Option].void(fa.traverse(fn)) == fa.traverse_(fn))
    }
  }

  test(s"Traverse[$name].updated_") {
    forAll { (fa: F[Int], i: Int, b: Int) =>
      val updatedThenToList: Option[List[Int]] = fa.updated_(i.toLong, b).fmap(_.toList)
      val toListThenUpdated: Option[List[Int]] = scala.util.Try(fa.toList.updated(i, b)).toOption
      assertEquals(updatedThenToList, toListThenUpdated)
    }
  }

}

object TraverseSuite {
  // forces testing of the underlying implementation (avoids overridden methods)
  abstract class Underlying[F[_]: Traverse](name: String)(implicit ArbFInt: Arbitrary[F[Int]])
      extends TraverseSuite(s"$name (underlying)")(proxyTraverse[F], ArbFInt)

  // proxies a traverse instance so we can test default implementations
  // to achieve coverage using default datatype instances
  private def proxyTraverse[F[_]: Traverse]: Traverse[F] =
    new Traverse[F] {
      def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
        Traverse[F].foldLeft(fa, b)(f)
      def foldRight[A, B](fa: F[A], lb: cats.Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Traverse[F].foldRight(fa, lb)(f)
      def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
        Traverse[F].traverse(fa)(f)
    }
}

class TraverseListSuite extends TraverseSuite[List]("List")
class TraverseVectorSuite extends TraverseSuite[Vector]("Vector")

@annotation.nowarn("cat=deprecation")
class TraverseStreamSuite extends TraverseSuite[Stream]("Stream")

class TraverseListSuiteUnderlying extends TraverseSuite.Underlying[List]("List")
class TraverseVectorSuiteUnderlying extends TraverseSuite.Underlying[Vector]("Vector")

@annotation.nowarn("cat=deprecation")
class TraverseStreamSuiteUnderlying extends TraverseSuite.Underlying[Stream]("Stream")

class TraverseSuiteAdditional
    extends CatsSuite
    with ScalaVersionSpecificTraverseSuite
    with TraverseSuiteAdditionalStreamSpecific {

  def checkZipWithIndexedStackSafety[F[_]](fromRange: Range => F[Int])(implicit F: Traverse[F]): Unit = {
    F.zipWithIndex(fromRange(1 to 70000))
    ()
  }

  test("Traverse[List].zipWithIndex stack safety") {
    checkZipWithIndexedStackSafety[List](_.toList)
  }

  test("Traverse[Vector].zipWithIndex stack safety") {
    checkZipWithIndexedStackSafety[Vector](_.toVector)
  }
}

@annotation.nowarn("cat=deprecation")
sealed trait TraverseSuiteAdditionalStreamSpecific { self: TraverseSuiteAdditional =>
  test("Traverse[Stream].zipWithIndex stack safety") {
    checkZipWithIndexedStackSafety[Stream](_.toStream)
  }
}
