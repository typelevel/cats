package cats
package kernel

private[kernel] object ScalaVersionSpecificLazyListCompat extends LazyListCompatBase {
  override final type T[A] = scala.collection.immutable.LazyList[A]

  override final def apply[A](a: A*): T[A] =
    scala.collection.immutable.LazyList.apply[A](a: _*)
}
