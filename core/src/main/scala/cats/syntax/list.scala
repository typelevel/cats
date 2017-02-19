package cats
package syntax

import cats.data.Validated.{Invalid, Valid}
import cats.data.{Ior, IorNel, NonEmptyList, ValidatedNel}

trait ListSyntax {
  implicit def catsSyntaxList[A](la: List[A]): ListOps[A] = new ListOps(la)
}

final class ListOps[A](val la: List[A]) extends AnyVal {
  def toNel: Option[NonEmptyList[A]] = NonEmptyList.fromList(la)

  /**
    * Returns a IorNel from a List
    */
  def toRightIorNel[B](ifEmpty: => B): IorNel[B, A] =
    toNel.fold[IorNel[B, A]](Ior.leftNel(ifEmpty))(Ior.right)

  /**
    * Returns a IorNel from a List
    */
  def toLeftIorNel[B](ifEmpty: => B): IorNel[A, B] =
    toNel.fold[IorNel[A, B]](Ior.rightNel(ifEmpty))(Ior.left)

  /**
    * Returns a ValidatedNel from a List
    */
  def toValidatedNel[B](ifEmpty: => B): ValidatedNel[A, B] =
    toNel.fold[ValidatedNel[A, B]](Valid(ifEmpty))(Invalid(_))
}
