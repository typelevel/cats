package cats
package kernel

private[kernel] object ScalaVersionSpecificLazyListCompat extends LazyListCompatBase {
  override final type T[A] = scala.collection.immutable.Stream[A]

  override final def apply[A](a: A*): T[A] =
    scala.collection.immutable.Stream.apply[A](a: _*)
}
