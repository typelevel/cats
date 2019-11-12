package cats
package data

abstract private[data] class ScalaVersionSpecificPackage {
  type NonEmptyLazyList[+A] = NonEmptyLazyList.Type[A]
  @deprecated("Use NonEmptyLazyList", "2.0.0-RC2")
  type NonEmptyStream[A] = OneAnd[Stream, A]

  @deprecated("Use NonEmptyLazyList", "2.0.0-RC2")
  def NonEmptyStream[A](head: A, tail: Stream[A]): NonEmptyStream[A] =
    OneAnd(head, tail)

  @deprecated("Use NonEmptyLazyList", "2.0.0-RC2")
  def NonEmptyStream[A](head: A, tail: A*): NonEmptyStream[A] =
    OneAnd(head, tail.toStream)
}
