package cats.tests

import cats.{Applicative, Eval, Traverse}
import cats.kernel.compat.scalaVersionSpecific._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.traverse._
import org.scalacheck.Arbitrary
import cats.syntax.eq._
import org.scalacheck.Prop._

@suppressUnusedImportWarningForScalaVersionSpecific
abstract class TraverseSuite[F[_]: Traverse](name: String)(implicit ArbFInt: Arbitrary[F[Int]]) extends CatsSuite {

  test(s"Traverse[$name].zipWithIndex") {
    forAll { (fa: F[Int]) =>
      assert(fa.zipWithIndex.toList === (fa.toList.zipWithIndex))
    }
  }

  test(s"Traverse[$name].mapWithIndex") {
    forAll { (fa: F[Int], fn: ((Int, Int)) => Int) =>
      assert(fa.mapWithIndex((a, i) => fn((a, i))).toList === (fa.toList.zipWithIndex.map(fn)))
    }
  }

  test(s"Traverse[$name].traverseWithIndexM") {
    forAll { (fa: F[Int], fn: ((Int, Int)) => (Int, Int)) =>
      val left = fa.traverseWithIndexM((a, i) => fn((a, i))).fmap(_.toList)
      val (xs, values) = fa.toList.zipWithIndex.map(fn).unzip
      assert(left === ((xs.combineAll, values)))
    }
  }

  test(s"Traverse[$name].mapAccumL") {
    forAll { (fa: F[Int], start: Long, fn: (Long, Int) => (Long, Int)) =>
      val (act, fb) = fa.mapAccumL(start)(fn)
      val (exp, xs) = fa.toList.foldLeft((start, List.empty[Int])) { case ((prev, acc), a) =>
        val (next, b) = fn(prev, a)
        (next, b :: acc)
      }
      assert(act === exp)
      assert(fb.toList === xs.reverse)
    }
  }

  test(s"Traverse[$name].traverse matches traverse_ with Option") {
    forAll { (fa: F[Int], fn: Int => Option[Int]) =>
      assert(Applicative[Option].void(fa.traverse(fn)) == fa.traverse_(fn))
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
