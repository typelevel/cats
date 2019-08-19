package cats
package data

import kernel.compat.scalaVersionSpecific._

abstract private[data] class ScalaVersionSpecificPackage {
  type NonEmptyLazyList[+A] = NonEmptyLazyList.Type[A]
  @deprecated("2.0.0-RC2", "Use NonEmptyLazyList")
  type NonEmptyStream[A] = OneAnd[Stream, A]

  @deprecated("2.0.0-RC2", "Use NonEmptyLazyList")
  def NonEmptyStream[A](head: A, tail: Stream[A]): NonEmptyStream[A] =
    OneAnd(head, tail)

  @suppressUnusedImportWarningForScalaVersionSpecific
  @deprecated("2.0.0-RC2", "Use NonEmptyLazyList")
  def NonEmptyStream[A](head: A, tail: A*): NonEmptyStream[A] =
    OneAnd(head, tail.toStream)
}
