package cats
package syntax

import cats.data.{ Xor, Validated, ValidatedNel }

trait OptionSyntax {
  def none[A] = Option.empty[A]
  implicit def optionIdSyntax[A](a: A): OptionIdOps[A] = new OptionIdOps(a)
  implicit def optionSyntax[A](oa: Option[A]): OptionOps[A] = new OptionOps(oa)
}

final class OptionIdOps[A](val a: A) extends AnyVal {
  def some: Option[A] = Option(a)
}

final class OptionOps[A](val oa: Option[A]) extends AnyVal {
  def toLeftXor[B](b: => B): A Xor B = oa.fold[A Xor B](Xor.Right(b))(Xor.Left(_))
  def toRightXor[B](b: => B): B Xor A = oa.fold[B Xor A](Xor.Left(b))(Xor.Right(_))
  def toInvalid[B](b: => B): Validated[A, B] = oa.fold[Validated[A, B]](Validated.Valid(b))(Validated.Invalid(_))
  def toInvalidNel[B](b: => B): ValidatedNel[A, B] = oa.fold[ValidatedNel[A, B]](Validated.Valid(b))(Validated.invalidNel(_))
  def toValid[B](b: => B): Validated[B, A] = oa.fold[Validated[B, A]](Validated.Invalid(b))(Validated.Valid(_))
  def toValidNel[B](b: => B): ValidatedNel[B, A] = oa.fold[ValidatedNel[B, A]](Validated.invalidNel(b))(Validated.Valid(_))
  def orEmpty(implicit A: Monoid[A]): A = oa.getOrElse(A.empty)
}
