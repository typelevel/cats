package cats.compat

object lazyList {
  def toLazyList[A](io: IterableOnce[A]): LazyList[A] = LazyList.from(io)

  def lazyListString: String = "LazyList"
}
