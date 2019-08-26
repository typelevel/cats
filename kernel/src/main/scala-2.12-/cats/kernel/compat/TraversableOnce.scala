package cats.kernel
package compat

@deprecated("No longer needed. Kept for bin compat", "2.0.0-RC1")
private[kernel] object TraversableOnce {
  def reduceOption[A, A1 >: A](as: TraversableOnce[A], op: (A1, A1) => A1): Option[A1] =
    as.reduceOption(op)
}
