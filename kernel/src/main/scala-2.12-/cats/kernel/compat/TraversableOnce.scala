package cats.kernel
package compat


private[kernel] object TraversableOnce  {
  def reduceOption[A, A1 >: A](as: TraversableOnce[A], op: (A1, A1) => A1): Option[A1] =
    as.reduceOption(op)
}
