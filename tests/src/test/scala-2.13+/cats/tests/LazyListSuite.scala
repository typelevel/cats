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
import org.scalatest.funsuite.AnyFunSuiteLike

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

  // Can't test applicative laws as they don't terminate
  checkAll("ZipLazyList[Int]", CommutativeApplyTests[ZipLazyList].apply[Int, Int, Int])

  test("show") {
    LazyList(1, 2, 3).show should ===(s"LazyList(1, ?)")
    LazyList.empty[Int].show should ===(s"LazyList()")
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
        initialShow should ===(unevaluatedLL.show)
      } else {
        lazyList.show should ===(lazyList.toString)
      }
    }
  }

}

final class LazyListInstancesSuite extends AnyFunSuiteLike {

  test("parallel instance in cats.instances.lazyList") {
    import cats.instances.lazyList._
    import cats.syntax.parallel._

    (LazyList(1, 2, 3), LazyList("A", "B", "C")).parTupled
  }
}
