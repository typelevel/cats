package cats.compat

object lazyList {


  def toLazyList[A](traversableOnce: TraversableOnce[A]): Stream[A] = traversableOnce.toStream

  def lazyListString: String = "Stream"

}
