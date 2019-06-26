package cats.kernel.compat

private[cats] object scalaVersionSpecific {
  type LazyList[+A] = Stream[A]
  val LazyList = Stream
  type IterableOnce[+A] = TraversableOnce[A]

  implicit class traversableOnceExtension[A](private val to: TraversableOnce[A]) extends AnyVal {
    def iterator: Iterator[A] = to.toIterator
  }
}
