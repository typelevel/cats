package cats
package syntax

import cats.data.{Ior, OptionT, Validated, ValidatedNec, ValidatedNel}
import cats.syntax.OptionOps.LiftToPartiallyApplied

trait OptionSyntax {
  final def none[A]: Option[A] = Option.empty[A]
  implicit final def catsSyntaxOptionId[A](a: A): OptionIdOps[A] = new OptionIdOps(a)
  implicit final def catsSyntaxOption[A](oa: Option[A]): OptionOps[A] = new OptionOps(oa)
}

final class OptionIdOps[A](private val a: A) extends AnyVal {

  /**
   * Wrap a value in `Some`.
   *
   * `3.some` is equivalent to `Some(3)`, but the former will have an inferred
   * return type of `Option[Int]` while the latter will have `Some[Int]`.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> 3.some
   * res0: Option[Int] = Some(3)
   * }}}
   */
  def some: Option[A] = Some(a)
}

final class OptionOps[A](private val oa: Option[A]) extends AnyVal {

  /**
   * If the `Option` is a `Some`, return its value in a [[cats.data.Validated.Invalid]].
   * If the `Option` is `None`, return the provided `B` value in a
   * [[cats.data.Validated.Valid]].
   *
   * Example:
   * {{{
   * scala> import cats.data.Validated
   * scala> import cats.implicits._
   *
   * scala> val error1: Option[String] = Some("error!")
   * scala> error1.toInvalid(3)
   * res0: Validated[String, Int] = Invalid(error!)
   *
   * scala> val error2: Option[String] = None
   * scala> error2.toInvalid(3)
   * res1: Validated[String, Int] = Valid(3)
   * }}}
   */
  def toInvalid[B](b: => B): Validated[A, B] = oa.fold[Validated[A, B]](Validated.Valid(b))(Validated.Invalid(_))

  /**
   * If the `Option` is a `Some`, wrap its value in a [[cats.data.NonEmptyList]]
   * and return it in a [[cats.data.Validated.Invalid]].
   * If the `Option` is `None`, return the provided `B` value in a
   * [[cats.data.Validated.Valid]].
   *
   * Example:
   * {{{
   * scala> import cats.data.ValidatedNel
   * scala> import cats.implicits._
   *
   * scala> val error1: Option[String] = Some("error!")
   * scala> error1.toInvalidNel(3)
   * res0: ValidatedNel[String, Int] = Invalid(NonEmptyList(error!))
   *
   * scala> val error2: Option[String] = None
   * scala> error2.toInvalidNel(3)
   * res1: ValidatedNel[String, Int] = Valid(3)
   * }}}
   */
  def toInvalidNel[B](b: => B): ValidatedNel[A, B] =
    oa.fold[ValidatedNel[A, B]](Validated.Valid(b))(Validated.invalidNel)

  /**
   * If the `Option` is a `Some`, wrap its value in a [[cats.data.Chain]]
   * and return it in a [[cats.data.Validated.Invalid]].
   * If the `Option` is `None`, return the provided `B` value in a
   * [[cats.data.Validated.Valid]].
   *
   * Example:
   * {{{
   * scala> import cats.data.ValidatedNec
   * scala> import cats.implicits._
   *
   * scala> val error1: Option[String] = Some("error!")
   * scala> error1.toInvalidNec(3)
   * res0: ValidatedNec[String, Int] = Invalid(Chain(error!))
   *
   * scala> val error2: Option[String] = None
   * scala> error2.toInvalidNec(3)
   * res1: ValidatedNec[String, Int] = Valid(3)
   * }}}
   */
  def toInvalidNec[B](b: => B): ValidatedNec[A, B] =
    oa.fold[ValidatedNec[A, B]](Validated.Valid(b))(Validated.invalidNec)

  /**
   * If the `Option` is a `Some`, return its value in a [[cats.data.Validated.Valid]].
   * If the `Option` is `None`, return the provided `B` value in a
   * [[cats.data.Validated.Invalid]].
   *
   * Example:
   * {{{
   * scala> import cats.data.Validated
   * scala> import cats.implicits._
   *
   * scala> val result1: Option[Int] = Some(3)
   * scala> result1.toValid("error!")
   * res0: Validated[String, Int] = Valid(3)
   *
   * scala> val result2: Option[Int] = None
   * scala> result2.toValid("error!")
   * res1: Validated[String, Int] = Invalid(error!)
   * }}}
   */
  def toValid[B](b: => B): Validated[B, A] = oa.fold[Validated[B, A]](Validated.Invalid(b))(Validated.Valid(_))

  /**
   * If the `Option` is a `Some`, return its value in a [[cats.data.Validated.Valid]].
   * If the `Option` is `None`, wrap the provided `B` value in a [[cats.data.NonEmptyList]]
   * and return the result in a [[cats.data.Validated.Invalid]].
   *
   * Example:
   * {{{
   * scala> import cats.data.ValidatedNel
   * scala> import cats.implicits._
   *
   * scala> val result1: Option[Int] = Some(3)
   * scala> result1.toValidNel("error!")
   * res0: ValidatedNel[String, Int] = Valid(3)
   *
   * scala> val result2: Option[Int] = None
   * scala> result2.toValidNel("error!")
   * res1: ValidatedNel[String, Int] = Invalid(NonEmptyList(error!))
   * }}}
   */
  def toValidNel[B](b: => B): ValidatedNel[B, A] =
    oa.fold[ValidatedNel[B, A]](Validated.invalidNel(b))(Validated.Valid(_))

  /**
   * If the `Option` is a `Some`, return its value in a [[cats.data.Validated.Valid]].
   * If the `Option` is `None`, wrap the provided `B` value in a [[cats.data.Chain]]
   * and return the result in a [[cats.data.Validated.Invalid]].
   *
   * Example:
   * {{{
   * scala> import cats.data.ValidatedNec
   * scala> import cats.implicits._
   *
   * scala> val result1: Option[Int] = Some(3)
   * scala> result1.toValidNec("error!")
   * res0: ValidatedNec[String, Int] = Valid(3)
   *
   * scala> val result2: Option[Int] = None
   * scala> result2.toValidNec("error!")
   * res1: ValidatedNec[String, Int] = Invalid(Chain(error!))
   * }}}
   */
  def toValidNec[B](b: => B): ValidatedNec[B, A] =
    oa.fold[ValidatedNec[B, A]](Validated.invalidNec(b))(Validated.Valid(_))

  /**
   * If the `Option` is a `Some`, return its value in a [[cats.data.Ior.Right]].
   * If the `Option` is `None`, wrap the provided `B` value in a [[cats.data.Ior.Left]]
   *
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.implicits._
   *
   * scala> val result1: Option[Int] = Some(3)
   * scala> result1.toRightIor("error!")
   * res0: Ior[String, Int] = Right(3)
   *
   * scala> val result2: Option[Int] = None
   * scala> result2.toRightIor("error!")
   * res1: Ior[String, Int] = Left(error!)
   * }}}
   */
  def toRightIor[B](b: => B): Ior[B, A] = oa.fold[Ior[B, A]](Ior.Left(b))(Ior.Right(_))

  /**
   * If the `Option` is a `Some`, return its value in a [[cats.data.Ior.Left]].
   * If the `Option` is `None`, wrap the provided `B` value in a [[cats.data.Ior.Right]]
   *
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.implicits._
   *
   * scala> val result1: Option[String] = Some("error!")
   * scala> result1.toLeftIor(3)
   * res0: Ior[String, Int] = Left(error!)
   *
   * scala> val result2: Option[String] = None
   * scala> result2.toLeftIor(3)
   * res1: Ior[String, Int] = Right(3)
   * }}}
   */
  def toLeftIor[B](b: => B): Ior[A, B] = oa.fold[Ior[A, B]](Ior.Right(b))(Ior.Left(_))

  /**
   * If the `Option` is a `Some`, return its value. If the `Option` is `None`,
   * return the `empty` value for `Monoid[A]`.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val someString: Option[String] = Some("hello")
   * scala> someString.orEmpty
   * res0: String = hello
   *
   * scala> val noneString: Option[String] = None
   * scala> noneString.orEmpty
   * res1: String = ""
   * }}}
   */
  def orEmpty(implicit A: Monoid[A]): A = oa.getOrElse(A.empty)

  /**
   * Lift to a F[A] as long as it has an ApplicativeError[F] instance
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> Some(1).liftTo[Either[CharSequence, ?]]("Empty")
   * res0: scala.Either[CharSequence, Int] = Right(1)
   *
   * scala> Option.empty[Int].liftTo[Either[CharSequence, ?]]("Empty")
   * res1: scala.Either[CharSequence, Int] = Left(Empty)
   * }}}
   */
  def liftTo[F[_]]: LiftToPartiallyApplied[F, A] = new LiftToPartiallyApplied(oa)

  /**
   * Transform the `Option` into a [[cats.data.OptionT]] while lifting it into the specified Applicative.
   *
   * {{{
   * scala> import cats.implicits._
   * scala> val op: Option[Int] = Some(3)
   * scala> op.toOptionT[List]
   * res0: cats.data.OptionT[List, Int] = OptionT(List(Some(3)))
   * }}}
   */
  def toOptionT[F[_]: Applicative]: OptionT[F, A] = OptionT.fromOption(oa)
}

object OptionOps {
  final private[syntax] class LiftToPartiallyApplied[F[_], A](oa: Option[A]) {
    def apply[E](ifEmpty: => E)(implicit F: ApplicativeError[F, _ >: E]): F[A] =
      ApplicativeError.liftFromOption(oa, ifEmpty)
  }
}
