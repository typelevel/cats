package cats
package tests

import cats.data.{NonEmptyLazyList, ZipLazyList}
import cats.instances.lazyList._
import cats.laws.discipline.NonEmptyParallelTests
import cats.laws.discipline.arbitrary._

trait ScalaVersionSpecificFoldableSuite { self: FoldableSuiteAdditional =>
  test("Foldable[LazyList].foldM stack safety") {
    checkMonadicFoldsStackSafety[LazyList](_.to(LazyList))
  }

  test("Foldable[NonEmptyLazyList].foldM/existsM/forallM/findM/collectFirstSomeM stack safety") {
    checkMonadicFoldsStackSafety[NonEmptyLazyList](xs => NonEmptyLazyList(xs.head, xs.tail: _*))
  }

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
    dangerous.foldM(0)((acc, a) => if (a < 2) Some(acc + a) else None) should ===(None)
  }

  def foldableLazyListWithDefaultImpl = new Foldable[LazyList] {
    def foldLeft[A, B](fa: LazyList[A], b: B)(f: (B, A) => B): B =
      Foldable[LazyList].foldLeft(fa, b)(f)

    def foldRight[A, B](fa: LazyList[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      Foldable[LazyList].foldRight(fa, lb)(f)
  }

  test(".foldLeftM short-circuiting") {
    implicit val F = foldableLazyListWithDefaultImpl
    val ns = LazyList.continually(1)
    val res = F.foldLeftM[Either[Int, *], Int, Int](ns, 0) { (sum, n) =>
      if (sum >= 100000) Left(sum) else Right(sum + n)
    }
    assert(res == Left(100000))
  }

  test(".foldLeftM short-circuiting optimality") {
    implicit val F = foldableLazyListWithDefaultImpl

    // test that no more elements are evaluated than absolutely necessary

    def concatUntil(ss: LazyList[String], stop: String): Either[String, String] =
      F.foldLeftM[Either[String, *], String, String](ss, "") { (acc, s) =>
        if (s == stop) Left(acc) else Right(acc + s)
      }

    assert(concatUntil("STOP" #:: boomLazyList[String], "STOP") == Left(""))
    assert(concatUntil("Zero" #:: "STOP" #:: boomLazyList[String], "STOP") == Left("Zero"))
    assert(concatUntil("Zero" #:: "One" #:: "STOP" #:: boomLazyList[String], "STOP") == Left("ZeroOne"))
  }

  test(".existsM/.forallM short-circuiting") {
    implicit val F = foldableLazyListWithDefaultImpl
    assert(F.existsM[Id, Boolean](true #:: boomLazyList[Boolean])(identity) == true)
    assert(F.forallM[Id, Boolean](false #:: boomLazyList[Boolean])(identity) == false)
  }

  test(".findM/.collectFirstSomeM short-circuiting") {
    implicit val F = foldableLazyListWithDefaultImpl
    assert((1 #:: boomLazyList[Int]).findM[Id](_ > 0) == Some(1))
    assert((1 #:: boomLazyList[Int]).collectFirstSomeM[Id, Int](Option.apply) == Some(1))
  }
}

trait ScalaVersionSpecificParallelSuite { self: ParallelSuite =>
  test("ParMap over LazyList should be consistent with zip") {
    forAll { (as: LazyList[Int], bs: LazyList[Int], cs: LazyList[Int]) =>
      val zipped = as
        .zip(bs)
        .map {
          case (a, b) => a + b
        }
        .zip(cs)
        .map {
          case (a, b) => a + b
        }

      (as, bs, cs).parMapN(_ + _ + _) should ===(zipped)
    }
  }

  test("ParTupled of LazyList should be consistent with ParMap of Tuple.apply") {
    forAll { (fa: LazyList[Int], fb: LazyList[Int], fc: LazyList[Int], fd: LazyList[Int]) =>
      (fa, fb, fc, fd).parTupled should ===((fa, fb, fc, fd).parMapN(Tuple4.apply))
    }
  }

  test("ParTupled of LazyList should be consistent with zip") {
    forAll { (fa: LazyList[Int], fb: LazyList[Int], fc: LazyList[Int], fd: LazyList[Int]) =>
      (fa, fb, fc, fd).parTupled should ===(fa.zip(fb).zip(fc).zip(fd).map { case (((a, b), c), d) => (a, b, c, d) })
    }
  }

  // Can't test Parallel here, as Applicative[ZipLazyList].pure doesn't terminate
  checkAll("Parallel[LazyList, ZipLazyList]",
           NonEmptyParallelTests[LazyList, ZipLazyList].nonEmptyParallel[Int, String])

  import NonEmptyLazyList.ZipNonEmptyLazyList

  checkAll("NonEmptyParallel[NonEmptyLazyList, OneAnd[ZipLazyList, *]",
           NonEmptyParallelTests[NonEmptyLazyList, ZipNonEmptyLazyList].nonEmptyParallel[Int, String])
}

trait ScalaVersionSpecificTraverseSuite { self: TraverseSuiteAdditional =>
  test("Traverse[LazyList].zipWithIndex stack safety") {
    checkZipWithIndexedStackSafety[LazyList](_.to(LazyList))
  }
}

class TraverseLazyListSuite extends TraverseSuite[LazyList]("LazyList")
class TraverseLazyListSuiteUnderlying extends TraverseSuite.Underlying[LazyList]("LazyList")
