package cats
package syntax

import cats.data.NonEmptyList

trait ListSyntax {
  implicit def catsSyntaxList[A](la: List[A]): ListOps[A] = new ListOps(la)
}

final class ListOps[A](val la: List[A]) extends AnyVal {
  def toNel: Option[NonEmptyList[A]] = NonEmptyList.fromList(la)
}
