package cats
package syntax

import cats.data.{ Validated, ValidatedNel }

trait ValidatedSyntax {
  implicit final def catsSyntaxValidatedId[A](a: A): ValidatedIdSyntax[A] = new ValidatedIdSyntax(a)
}

final class ValidatedIdSyntax[A](val a: A) extends AnyVal {
  def valid[B]: Validated[B, A] = Validated.Valid(a)
  def validNel[B]: ValidatedNel[B, A] = Validated.Valid(a)
  def invalid[B]: Validated[A, B] = Validated.Invalid(a)
  def invalidNel[B]: ValidatedNel[A, B] = Validated.invalidNel(a)
}
