package cats.tests

class FoldableLazyListSuite extends FoldableSuite[LazyList]("lazyList") {
  def iterator[T](list: LazyList[T]): Iterator[T] = list.iterator
}
