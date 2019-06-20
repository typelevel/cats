package cats.compat

object StreamOps {
  def toStream[A](traversableOnce: TraversableOnce[A]): Stream[A] = traversableOnce.toStream

  def emptyStream[A]: Stream[A] = Stream.empty[A]

  def streamString: String = "Stream"

}
