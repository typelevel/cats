package cats
package syntax

import cats.data.{Validated, ValidatedNec, ValidatedNel}

trait ValidatedSyntax {
  implicit final def catsSyntaxValidatedId[A](a: A): ValidatedIdSyntax[A] = new ValidatedIdSyntax(a)
}

final class ValidatedIdSyntax[A](private val a: A) extends AnyVal {
  def valid[B]: Validated[B, A] = Validated.Valid(a)
  def validNel[B]: ValidatedNel[B, A] = Validated.Valid(a)
  def invalid[B]: Validated[A, B] = Validated.Invalid(a)
  def invalidNel[B]: ValidatedNel[A, B] = Validated.invalidNel(a)
}

trait ValidatedExtensionSyntax {
  implicit final def catsSyntaxValidatedExtension[E, A](v: Validated[E, A]): ValidatedExtension[E, A] =
    new ValidatedExtension(v)
}

final class ValidatedExtension[E, A](private val self: Validated[E, A]) extends AnyVal {
  def liftTo[F[_]](implicit F: ApplicativeError[F, E]): F[A] =
    new ApplicativeErrorExtensionOps(F).fromValidated(self)
}

trait ValidatedSyntaxBincompat0 {
  implicit final def catsSyntaxValidatedIdBinCompat0[A](a: A): ValidatedIdOpsBinCompat0[A] =
    new ValidatedIdOpsBinCompat0(a)
}

final class ValidatedIdOpsBinCompat0[A](private val a: A) extends AnyVal {

  /**
   * Wrap a value to a valid ValidatedNec
   *
   * For example:
   * {{{
   * scala> import cats.implicits._, cats.data._
   * scala> 1.validNec[String]
   * res0: Validated[NonEmptyChain[String], Int] = Valid(1)
   * }}}
   */
  def validNec[B]: ValidatedNec[B, A] = Validated.Valid(a)

  /**
   * Wrap a value to an invalid ValidatedNec
   *
   * For example:
   * {{{
   * scala> import cats.implicits._, cats.data._
   * scala> "Err".invalidNec[Int]
   * res0: Validated[NonEmptyChain[String], Int] = Invalid(Chain(Err))
   * }}}
   */
  def invalidNec[B]: ValidatedNec[A, B] = Validated.invalidNec(a)
}
