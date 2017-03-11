package cats
package syntax

import cats.data.NonEmptyList

trait ListSyntax {
  implicit def catsSyntaxList[A](la: List[A]): ListOps[A] = new ListOps(la)
}

final class ListOps[A](val la: List[A]) extends AnyVal {

  /**
    * Returns an Option of NonEmptyList from a List
    *
    * Example:
    * {{{
    * scala> import cats.data.NonEmptyList
    * scala> import cats.implicits._
    *
    * scala> val result1: List[Int] = List(1, 2)
    * scala> result1.toNel
    * res0: Option[NonEmptyList[Int]] = Some(NonEmptyList(1, 2))
    *
    * scala> val result2: List[Int] = List.empty[Int]
    * scala> result2.toNel
    * res1: Option[NonEmptyList[Int]] = None
    * }}}
    */
  def toNel: Option[NonEmptyList[A]] = NonEmptyList.fromList(la)
}
