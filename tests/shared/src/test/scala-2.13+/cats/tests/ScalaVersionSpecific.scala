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

import cats.*
import cats.data.NonEmptyLazyList
import cats.laws.discipline.ExhaustiveCheck
import cats.laws.discipline.MiniInt
import cats.laws.discipline.NonEmptyParallelTests
import cats.laws.discipline.ParallelTests
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import cats.syntax.all.*
import org.scalacheck.Prop.*

trait ScalaVersionSpecificFoldableSuite { self: FoldableSuiteAdditional =>
  test("Foldable[LazyList] monadic folds stack safety")(checkMonadicFoldsStackSafety(_.to(LazyList)))
  test("Foldable[LazyList].slidingN stack safety")(checkSlidingNStackSafety(_.to(LazyList)))

  test("Foldable[NonEmptyLazyList] monadic folds stack safety")(
    checkMonadicFoldsStackSafety(xs => NonEmptyLazyList(xs.head, xs.tail: _*))
  )

  test("Foldable[NonEmptyLazyList].slidingN stack safety")(
    checkSlidingNStackSafety(xs => NonEmptyLazyList(xs.head, xs.tail: _*))
  )

  private def bombLazyList[A]: A = sys.error("boom")
  private val dangerousLazyList = 0 #:: 1 #:: 2 #:: bombLazyList[Int] #:: LazyList.empty
  private def boomLazyList[A]: LazyList[A] =
    bombLazyList[A] #:: LazyList.empty
  test("Foldable[LazyList] doesn't blow up") {

    // doesn't blow up - this also ensures it works for infinite streams.
    assert(contains(dangerousLazyList, 2).value)
  }

  test("Foldable[LazyList] lazy results don't blow up unless you call .value on them") {
    contains(dangerousLazyList, -1)
  }

  test("Foldable[LazyList] param to foldRight is actually being handled lazily") {
    // ensure that the . it only needs to be evaluated if we reach the
    // "end" of the fold.
    val trap = Eval.later(bombLazyList[Boolean])
    val result = Foldable[LazyList].foldRight(1 #:: 2 #:: LazyList.empty, trap) { (n, lb) =>
      if (n == 2) Now(true) else lb
    }
    assert(result.value)
  }

  test("Foldable[LazyList]  trampolining") {
    val large = LazyList((1 to 10000): _*)
    assert(contains(large, 10000).value)
  }

  test("Foldable[LazyList] laziness of foldM") {
    assert(dangerousLazyList.foldM(0)((acc, a) => if (a < 2) Some(acc + a) else None) === None)
  }

  def foldableLazyListWithDefaultImpl: Foldable[LazyList] =
    new Foldable[LazyList] {
      def foldLeft[A, B](fa: LazyList[A], b: B)(f: (B, A) => B): B =
        Foldable[LazyList].foldLeft(fa, b)(f)

      def foldRight[A, B](fa: LazyList[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable[LazyList].foldRight(fa, lb)(f)
    }

  test("Foldable[LazyList].foldLeftM short-circuiting") {
    implicit val F: Foldable[LazyList] = foldableLazyListWithDefaultImpl
    val ns = LazyList.continually(1)
    val res = F.foldLeftM[Either[Int, *], Int, Int](ns, 0) { (sum, n) =>
      if (sum >= 100000) Left(sum) else Right(sum + n)
    }
    assert(res == Left(100000))
  }

  test("Foldable[LazyList].foldLeftM short-circuiting optimality") {
    implicit val F: Foldable[LazyList] = foldableLazyListWithDefaultImpl

    // test that no more elements are evaluated than absolutely necessary

    def concatUntil(ss: LazyList[String], stop: String): Either[String, String] =
      F.foldLeftM[Either[String, *], String, String](ss, "") { (acc, s) =>
        if (s == stop) Left(acc) else Right(acc + s)
      }

    assert(concatUntil("STOP" #:: boomLazyList[String], "STOP") == Left(""))
    assert(concatUntil("Zero" #:: "STOP" #:: boomLazyList[String], "STOP") == Left("Zero"))
    assert(concatUntil("Zero" #:: "One" #:: "STOP" #:: boomLazyList[String], "STOP") == Left("ZeroOne"))
  }

  test("Foldable[LazyList].existsM/.forallM short-circuiting") {
    implicit val F: Foldable[LazyList] = foldableLazyListWithDefaultImpl
    assert(F.existsM[Id, Boolean](true #:: boomLazyList[Boolean])(identity) == true)
    assert(F.forallM[Id, Boolean](false #:: boomLazyList[Boolean])(identity) == false)
  }

  test("Foldable[LazyList].findM/.collectFirstSomeM short-circuiting") {
    implicit val F: Foldable[LazyList] = foldableLazyListWithDefaultImpl
    assert((1 #:: boomLazyList[Int]).findM[Id](_ > 0) == Some(1))
    assert((1 #:: boomLazyList[Int]).collectFirstSomeM[Id, Int](Option.apply) == Some(1))
  }

  test("#4244 ambiguous `contains_` syntax") {
    assertEquals(List("a").map(List("a").contains_), List(true))
  }
}

trait ScalaVersionSpecificParallelSuite { self: ParallelSuite =>
  test("ParMap over LazyList should be consistent with zip") {
    forAll { (as: LazyList[Int], bs: LazyList[Int], cs: LazyList[Int]) =>
      val zipped = as
        .zip(bs)
        .map { case (a, b) =>
          a + b
        }
        .zip(cs)
        .map { case (a, b) =>
          a + b
        }

      assert((as, bs, cs).parMapN(_ + _ + _) === zipped)
    }
  }

  test("ParTupled of LazyList should be consistent with ParMap of Tuple.apply") {
    forAll { (fa: LazyList[Int], fb: LazyList[Int], fc: LazyList[Int], fd: LazyList[Int]) =>
      assert((fa, fb, fc, fd).parTupled === ((fa, fb, fc, fd).parMapN(Tuple4.apply)))
    }
  }

  test("ParTupled of LazyList should be consistent with zip") {
    forAll { (fa: LazyList[Int], fb: LazyList[Int], fc: LazyList[Int], fd: LazyList[Int]) =>
      assert((fa, fb, fc, fd).parTupled === fa.zip(fb).zip(fc).zip(fd).map { case (((a, b), c), d) => (a, b, c, d) })
    }
  }

  // Can't test Parallel here, as Applicative[ZipLazyList].pure doesn't terminate
  checkAll("Parallel[LazyList]", NonEmptyParallelTests[LazyList].nonEmptyParallel[Int, String])

  checkAll("Parallel[NonEmptyLazyList]", ParallelTests[NonEmptyLazyList].parallel[Int, String])
}

trait ScalaVersionSpecificRegressionSuite { self: RegressionSuite =>
  test("#513: traverse short circuits - Either (for LazyList)") {
    var count = 0
    def validate(i: Int): Either[String, Int] = {
      count = count + 1
      if (i < 5) Either.right(i) else Either.left(s"$i is greater than 5")
    }

    def checkAndResetCount(expected: Int): Unit = {
      assert(count === expected)
      count = 0
    }

    assert(LazyList(1, 2, 6, 8).traverse(validate) === (Either.left("6 is greater than 5")))
    // shouldn't have ever evaluated validate(8)
    checkAndResetCount(3)

    assert(LazyList(1, 2, 6, 8).traverseVoid(validate) === Either.left("6 is greater than 5"))
    checkAndResetCount(3)
  }
}

trait ScalaVersionSpecificTraverseSuite { self: TraverseSuiteAdditional =>
  test("Traverse[LazyList].zipWithIndex stack safety") {
    checkZipWithIndexedStackSafety[LazyList](_.to(LazyList))
  }
}

trait ScalaVersionSpecificAlgebraInvariantSuite {

  // This version-specific instance is required since 2.12 and below do not have parseString on the Numeric class
  protected trait MiniIntNumeric extends Numeric[MiniInt] {
    def compare(x: MiniInt, y: MiniInt): Int = Order[MiniInt].compare(x, y)
    def plus(x: MiniInt, y: MiniInt): MiniInt = x + y
    def minus(x: MiniInt, y: MiniInt): MiniInt = x + (-y)
    def times(x: MiniInt, y: MiniInt): MiniInt = x * y
    def negate(x: MiniInt): MiniInt = -x
    def fromInt(x: Int): MiniInt = MiniInt.unsafeFromInt(x)
    def toInt(x: MiniInt): Int = x.toInt
    def toLong(x: MiniInt): Long = x.toInt.toLong
    def toFloat(x: MiniInt): Float = x.toInt.toFloat
    def toDouble(x: MiniInt): Double = x.toInt.toDouble
    def parseString(str: String): Option[MiniInt] = Integral[Int].parseString(str).flatMap(MiniInt.fromInt)
  }

  // This version-specific instance is required since 2.12 and below do not have parseString on the Numeric class
  implicit protected def eqNumeric[A: Eq: ExhaustiveCheck]: Eq[Numeric[A]] = Eq.by { numeric =>
    // This allows us to catch the case where the fromInt overflows. We use the None to compare two Numeric instances,
    // verifying that when fromInt throws for one, it throws for the other.
    val fromMiniInt: MiniInt => Option[A] =
      miniInt =>
        try Some(numeric.fromInt(miniInt.toInt))
        catch {
          case _: IllegalArgumentException => None // MiniInt overflow
        }

    val parseMiniIntStrings: Option[MiniInt] => Option[A] = {
      case Some(miniInt) => numeric.parseString(miniInt.toInt.toString)
      case None          => numeric.parseString("invalid") // Use this to test parsing of non-numeric strings
    }

    (
      numeric.compare _,
      numeric.plus _,
      numeric.minus _,
      numeric.times _,
      numeric.negate _,
      fromMiniInt,
      numeric.toInt _,
      numeric.toLong _,
      numeric.toFloat _,
      numeric.toDouble _,
      parseMiniIntStrings
    )
  }

}

class TraverseLazyListSuite extends TraverseSuite[LazyList]("LazyList")
class TraverseLazyListSuiteUnderlying extends TraverseSuite.Underlying[LazyList]("LazyList")
class TraverseFilterLazyListSuite extends TraverseFilterSuite[LazyList]("LazyList")
