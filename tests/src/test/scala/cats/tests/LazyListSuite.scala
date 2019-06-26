package cats
package tests

import cats.laws.discipline.{
  AlternativeTests,
  CoflatMapTests,
  CommutativeApplyTests,
  MonadTests,
  SemigroupalTests,
  SerializableTests,
  TraverseFilterTests,
  TraverseTests
}
import cats.data.ZipStream
import cats.laws.discipline.arbitrary._
import kernel.compat.scalaVersionSpecific._
import compat.lazyList.lazyListString

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

  // Can't test applicative laws as they don't terminate
  checkAll("ZipStream[Int]", CommutativeApplyTests[ZipStream].apply[Int, Int, Int])

  test("show") {
    LazyList(1, 2, 3).show should ===(s"$lazyListString(1, ?)")
    LazyList.empty[Int].show should ===(s"$lazyListString()")
  }

  test("Show[Stream] is referentially transparent, unlike Stream.toString") {
    forAll { lazyList: LazyList[Int] =>
      if (!lazyList.isEmpty) {
        val unevaluatedLL = lazyList.map(identity)
        val initialShow = unevaluatedLL.show

        // Evaluating the tail can cause Stream.toString to return different values,
        // depending on the internal state of the Stream. Show[Stream] should return
        // consistent values independent of internal state.
        unevaluatedLL.tail
        initialShow should ===(unevaluatedLL.show)
      } else {
        lazyList.show should ===(lazyList.toString)
      }
    }
  }

}
