package cats
package data

import kernel.compat.scalaVersionSpecific._

abstract private[data] class ScalaVersionSpecificPackage {
  type NonEmptyStream[A] = OneAnd[Stream, A]

  def NonEmptyStream[A](head: A, tail: Stream[A] = Stream.empty): NonEmptyStream[A] =
    OneAnd(head, tail)

  @suppressUnusedImportWarningForScalaVersionSpecific
  def NonEmptyStream[A](head: A, tail: A*): NonEmptyStream[A] =
    OneAnd(head, tail.toStream)
}
