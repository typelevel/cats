package cats.kernel.compat

object lazyList {
  type LazyList[+A] = Stream[A]
  val LazyList = Stream
}
