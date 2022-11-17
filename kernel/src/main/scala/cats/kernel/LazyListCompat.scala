package cats
package kernel

private[kernel] trait LazyListCompatBase{
  type T[A]

  def apply[A](a: A*): T[A]

  final def empty[A]: T[A] = apply[A]()
}
