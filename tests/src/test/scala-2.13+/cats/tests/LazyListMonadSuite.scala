package cats
package tests

import cats.laws.discipline.MonadTests
import cats.data.NonEmptyStream
import cats.laws.discipline.arbitrary._

class LazyListMonadSuite extends CatsSuite {

  //todo: fix Monad[LazyList] implementation to make it stack safe
  checkAll("LazyList[Int]", MonadTests[LazyList].monad[Int, Int, Int])
  checkAll("NonEmptyStream[Int]", MonadTests[NonEmptyStream].stackUnsafeMonad[Int, Int, Int])
}
