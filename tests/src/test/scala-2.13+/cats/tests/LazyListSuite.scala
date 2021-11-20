package cats.tests

import cats.{Align, Alternative, CoflatMap, Monad, Semigroupal, Traverse, TraverseFilter}
import cats.data.ZipLazyList
import cats.laws.discipline.{
  AlignTests,
  AlternativeTests,
  CoflatMapTests,
  CommutativeApplyTests,
  MonadTests,
  SemigroupalTests,
  SerializableTests,
  ShortCircuitingTests,
  TraverseFilterTests,
  TraverseTests
}
import cats.laws.discipline.arbitrary._
import cats.syntax.show._
import cats.syntax.eq._
import cats.syntax.foldable._
import org.scalacheck.Prop._

import scala.util.control.TailCalls

class LazyListSuite extends CatsSuite {
  checkAll("LazyList[Int]", SemigroupalTests[LazyList].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[LazyList]", SerializableTests.serializable(Semigroupal[LazyList]))

  checkAll("LazyList[Int]", CoflatMapTests[LazyList].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[LazyList]", SerializableTests.serializable(CoflatMap[LazyList]))

  checkAll("LazyList[Int]", AlternativeTests[LazyList].alternative[Int, Int, Int])
  checkAll("Alternative[LazyList]", SerializableTests.serializable(Alternative[LazyList]))

  checkAll("LazyList[Int]", MonadTests[LazyList].monad[Int, Int, Int])
  checkAll("Monad[LazyList]", SerializableTests.serializable(Monad[LazyList]))

  checkAll("LazyList[Int] with Option", TraverseTests[LazyList].traverse[Int, Int, Int, Set[Int], Option, Option])
  checkAll("Traverse[LazyList]", SerializableTests.serializable(Traverse[LazyList]))

  checkAll("LazyList[Int]", TraverseFilterTests[LazyList].traverseFilter[Int, Int, Int])
  checkAll("TraverseFilter[LazyList]", SerializableTests.serializable(TraverseFilter[LazyList]))

  checkAll("LazyList[Int]", AlignTests[LazyList].align[Int, Int, Int, Int])
  checkAll("Align[LazyList]", SerializableTests.serializable(Align[LazyList]))

  checkAll("LazyList[Int]", ShortCircuitingTests[LazyList].foldable[Int])
  checkAll("LazyList[Int]", ShortCircuitingTests[LazyList].traverseFilter[Int])

  // Can't test applicative or selective laws as they don't terminate
  checkAll("ZipLazyList[Int]", CommutativeApplyTests[ZipLazyList].apply[Int, Int, Int])

  test("show") {
    assert(LazyList(1, 2, 3).show === s"LazyList(1, ?)")
    assert(LazyList.empty[Int].show === s"LazyList()")
  }

  test("Avoid all evaluation of LazyList#foldRightDefer") {
    val sum = LazyList
      .from(1)
      .foldRightDefer(TailCalls.done(0)) { (elem, acc) =>
        if (elem <= 100) acc.map(_ + elem) else TailCalls.done(0)
      }
      .result
    (1 to 100).sum === sum
  }

  test("Show[LazyList] is referentially transparent, unlike LazyList.toString") {
    forAll { (lazyList: LazyList[Int]) =>
      if (!lazyList.isEmpty) {
        val unevaluatedLL = lazyList.map(identity)
        val initialShow = unevaluatedLL.show

        // Evaluating the tail can cause LazyList.toString to return different values,
        // depending on the internal state of the LazyList. Show[LazyList] should return
        // consistent values independent of internal state.
        unevaluatedLL.tail
        assert(initialShow === (unevaluatedLL.show))
      } else {
        assert(lazyList.show === (lazyList.toString))
      }
    }
  }

}

final class LazyListInstancesSuite extends munit.FunSuite {

  test("parallel instance in cats.instances.lazyList") {
    import cats.instances.lazyList._
    import cats.syntax.parallel._

    (LazyList(1, 2, 3), LazyList("A", "B", "C")).parTupled
  }
}
