package cats
package tests

import org.scalacheck.Arbitrary

import cats.instances.all._
import kernel.compat.scalaVersionSpecific._

@suppressUnusedImportWarningForScalaVersionSpecific
abstract class TraverseSuite[F[_]: Traverse](name: String)(implicit ArbFInt: Arbitrary[F[Int]]) extends CatsSuite {

  test(s"Traverse[$name].zipWithIndex") {
    forAll { (fa: F[Int]) =>
      fa.zipWithIndex.toList should ===(fa.toList.zipWithIndex)
    }
  }

  test(s"Traverse[$name].mapWithIndex") {
    forAll { (fa: F[Int], fn: ((Int, Int)) => Int) =>
      fa.mapWithIndex((a, i) => fn((a, i))).toList should ===(fa.toList.zipWithIndex.map(fn))
    }
  }

  test(s"Traverse[$name].traverseWithIndexM") {
    forAll { (fa: F[Int], fn: ((Int, Int)) => (Int, Int)) =>
      val left = fa.traverseWithIndexM((a, i) => fn((a, i))).map(_.toList)
      val (xs, values) = fa.toList.zipWithIndex.map(fn).unzip
      left should ===((xs.combineAll, values))
    }
  }

}

object TraverseSuite {
  // forces testing of the underlying implementation (avoids overridden methods)
  abstract class Underlying[F[_]: Traverse](name: String)(implicit ArbFInt: Arbitrary[F[Int]])
      extends TraverseSuite(s"$name (underlying)")(proxyTraverse[F], ArbFInt)

  // proxies a traverse instance so we can test default implementations
  // to achieve coverage using default datatype instances
  private def proxyTraverse[F[_]: Traverse]: Traverse[F] = new Traverse[F] {
    def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
      Traverse[F].foldLeft(fa, b)(f)
    def foldRight[A, B](fa: F[A], lb: cats.Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      Traverse[F].foldRight(fa, lb)(f)
    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
      Traverse[F].traverse(fa)(f)
  }
}

class TraverseListSuite extends TraverseSuite[List]("List")
class TraverseStreamSuite extends TraverseSuite[Stream]("Stream")
class TraverseVectorSuite extends TraverseSuite[Vector]("Vector")

class TraverseListSuiteUnderlying extends TraverseSuite.Underlying[List]("List")
class TraverseStreamSuiteUnderlying extends TraverseSuite.Underlying[Stream]("Stream")
class TraverseVectorSuiteUnderlying extends TraverseSuite.Underlying[Vector]("Vector")

class TraverseSuiteAdditional extends CatsSuite with ScalaVersionSpecificTraverseSuite {

  def checkZipWithIndexedStackSafety[F[_]](fromRange: Range => F[Int])(implicit F: Traverse[F]): Unit = {
    F.zipWithIndex(fromRange(1 to 70000))
    ()
  }

  test("Traverse[List].zipWithIndex stack safety") {
    checkZipWithIndexedStackSafety[List](_.toList)
  }

  test("Traverse[Stream].zipWithIndex stack safety") {
    checkZipWithIndexedStackSafety[Stream](_.toStream)
  }

  test("Traverse[Vector].zipWithIndex stack safety") {
    checkZipWithIndexedStackSafety[Vector](_.toVector)
  }
}
