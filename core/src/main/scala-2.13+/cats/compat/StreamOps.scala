package cats.compat

object StreamOps {
  def toStream[A](io: IterableOnce[A]): LazyList[A] = LazyList.from(io)

  def emptyStream[A]: LazyList[A] = LazyList.empty[A]

  def streamString: String = "LazyList"
}
