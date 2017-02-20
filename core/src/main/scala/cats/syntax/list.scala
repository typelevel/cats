package cats
package syntax

import cats.data.Validated.{Invalid, Valid}
import cats.data.{Ior, IorNel, NonEmptyList, ValidatedNel}

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

  /**
    * Returns a IorNel from a List
    *
    * Example:
    * {{{
    * scala> import cats.data.IorNel
    * scala> import cats.implicits._
    *
    * scala> val result1: List[Int] = List(1, 2)
    * scala> result1.toRightIorNel("error!")
    * res0: IorNel[String, Int] = Right(NonEmptyList(1, 2))
    *
    * scala> val result2: List[Int] = List.empty[Int]
    * scala> result2.toRightIorNel("error!")
    * res1: IorNel[String, Int] = Left(NonEmptyList(error!))
    * }}}
    */
  def toRightIorNel[B](ifEmpty: => B): IorNel[B, A] =
    toNel.fold[IorNel[B, A]](Ior.leftNel(ifEmpty))(Ior.right)

  /**
    * Returns a IorNel from a List
    *
    * Example:
    * {{{
    * scala> import cats.data.IorNel
    * scala> import cats.implicits._
    *
    * scala> val result1: List[String] = List("error 1", "error 2")
    * scala> result1.toLeftIorNel(1)
    * res0: IorNel[String, Int] = Left(NonEmptyList(error 1, error 2))
    *
    * scala> val result2: List[String] = List.empty[String]
    * scala> result2.toLeftIorNel(1)
    * res1: IorNel[String, Int] = Right(NonEmptyList(1))
    * }}}
    */
  def toLeftIorNel[B](ifEmpty: => B): IorNel[A, B] =
    toNel.fold[IorNel[A, B]](Ior.rightNel(ifEmpty))(Ior.left)

  /**
    * Returns a ValidatedNel from a List
    *
    * Example:
    * {{{
    * scala> import cats.data.ValidatedNel
    * scala> import cats.implicits._
    *
    * scala> val result1: List[String] = List("error 1", "error 2")
    * scala> result1.toValidatedNel(1)
    * res0: ValidatedNel[String, Int] = Invalid(NonEmptyList(error 1, error 2))
    *
    * scala> val result2: List[String] = List.empty[String]
    * scala> result2.toValidatedNel(1)
    * res1: ValidatedNel[String, Int] = Valid(1)
    * }}}
    */
  def toValidatedNel[B](ifEmpty: => B): ValidatedNel[A, B] =
    toNel.fold[ValidatedNel[A, B]](Valid(ifEmpty))(Invalid(_))
}
