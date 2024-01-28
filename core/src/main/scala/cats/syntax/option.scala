/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats
package syntax

import cats.data.{
  EitherNec,
  EitherNel,
  Ior,
  NonEmptyChain,
  NonEmptyList,
  OptionT,
  Validated,
  ValidatedNec,
  ValidatedNel
}
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
   * scala> import cats.syntax.all._
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
   * scala> import cats.syntax.all._
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
   * scala> import cats.syntax.all._
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
   * scala> import cats.syntax.all._
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
   * scala> import cats.syntax.all._
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
   * scala> import cats.syntax.all._
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
   * scala> import cats.syntax.all._
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
   * scala> import cats.syntax.all._
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
   * scala> import cats.syntax.all._
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
   * If the `Option` is a `Some`, return its value in a [[scala.Right]].
   * If the `Option` is `None`, wrap the provided `B` value in a [[cats.data.NonEmptyList]]
   * and return the result in a [[scala.Left]].
   *
   * Example:
   * {{{
   * scala> import cats.data.EitherNel
   * scala> import cats.syntax.all._
   *
   * scala> val result1: Option[Int] = Some(3)
   * scala> result1.toRightNel("error!")
   * res0: EitherNel[String, Int] = Right(3)
   *
   * scala> val result2: Option[Int] = None
   * scala> result2.toRightNel("error!")
   * res1: EitherNel[String, Int] = Left(NonEmptyList(error!))
   * }}}
   */
  def toRightNel[B](b: => B): EitherNel[B, A] = oa.toRight(NonEmptyList.one(b))

  /**
   * If the `Option` is a `Some`, return its value in a [[scala.Right]].
   * If the `Option` is `None`, wrap the provided `B` value in a [[cats.data.NonEmptyChain]]
   * and return the result in a [[scala.Left]].
   *
   * Example:
   * {{{
   * scala> import cats.data.EitherNec
   * scala> import cats.syntax.all._
   *
   * scala> val result1: Option[Int] = Some(3)
   * scala> result1.toRightNec("error!")
   * res0: EitherNec[String, Int] = Right(3)
   *
   * scala> val result2: Option[Int] = None
   * scala> result2.toRightNec("error!")
   * res1: EitherNec[String, Int] = Left(Chain(error!))
   * }}}
   */
  def toRightNec[B](b: => B): EitherNec[B, A] = oa.toRight(NonEmptyChain.one(b))

  /**
   * If the `Option` is a `Some`, wrap its value in a [[cats.data.NonEmptyList]]
   * and return it in a [[scala.Left]].
   * If the `Option` is `None`, return the provided `B` value in a
   * [[scala.Right]].
   *
   * Example:
   * {{{
   * scala> import cats.data.EitherNel
   * scala> import cats.syntax.all._
   *
   * scala> val error1: Option[String] = Some("error!")
   * scala> error1.toLeftNel(3)
   * res0: EitherNel[String, Int] = Left(NonEmptyList(error!))
   *
   * scala> val error2: Option[String] = None
   * scala> error2.toLeftNel(3)
   * res1: EitherNel[String, Int] = Right(3)
   * }}}
   */
  def toLeftNel[B](b: => B): EitherNel[A, B] =
    oa.fold[EitherNel[A, B]](Right(b))(a => Left(NonEmptyList.one(a)))

  /**
   * If the `Option` is a `Some`, wrap its value in a [[cats.data.NonEmptyChain]]
   * and return it in a [[scala.Left]].
   * If the `Option` is `None`, return the provided `B` value in a
   * [[scala.Right]].
   *
   * Example:
   * {{{
   * scala> import cats.data.EitherNec
   * scala> import cats.syntax.all._
   *
   * scala> val error1: Option[String] = Some("error!")
   * scala> error1.toLeftNec(3)
   * res0: EitherNec[String, Int] = Left(Chain(error!))
   *
   * scala> val error2: Option[String] = None
   * scala> error2.toLeftNec(3)
   * res1: EitherNec[String, Int] = Right(3)
   * }}}
   */
  def toLeftNec[B](b: => B): EitherNec[A, B] =
    oa.fold[EitherNec[A, B]](Right(b))(a => Left(NonEmptyChain.one(a)))

  /**
   * If the `Option` is a `Some`, return its value. If the `Option` is `None`,
   * return the `empty` value for `Monoid[A]`.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
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
   * scala> import cats.syntax.all._
   * scala> Some(1).liftTo[Either[CharSequence, *]]("Empty")
   * res0: scala.Either[CharSequence, Int] = Right(1)
   *
   * scala> Option.empty[Int].liftTo[Either[CharSequence, *]]("Empty")
   * res1: scala.Either[CharSequence, Int] = Left(Empty)
   * }}}
   */
  def liftTo[F[_]]: LiftToPartiallyApplied[F, A] = new LiftToPartiallyApplied(oa)

  /**
   * Raise to an F[Unit], as long as it has an ApplicativeError[F, A] instance
   * If the option is empty, an empty unit effect is given.
   * If the option contains an error, it is raised.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> type F[A] = Either[String, A]
   *
   * scala> Option.empty[String].raiseTo[F]
   * res0: scala.Either[String, Unit] = Right(())
   *
   * scala> Option("Failed").raiseTo[F]
   * res1: scala.Either[String, Unit] = Left(Failed)
   * }}}
   */
  def raiseTo[F[_]](implicit F: ApplicativeError[F, A]): F[Unit] =
    oa.fold(F.unit)(F.raiseError)

  /**
   * Transform the `Option` into a [[cats.data.OptionT]] while lifting it into the specified Applicative.
   *
   * {{{
   * scala> import cats.syntax.all._
   * scala> val op: Option[Int] = Some(3)
   * scala> op.toOptionT[List]
   * res0: cats.data.OptionT[List, Int] = OptionT(List(Some(3)))
   * }}}
   */
  def toOptionT[F[_]: Applicative]: OptionT[F, A] = OptionT.fromOption(oa)
}

object OptionOps {
  final class LiftToPartiallyApplied[F[_], A](oa: Option[A]) {
    def apply[E](ifEmpty: => E)(implicit F: ApplicativeError[F, ? >: E]): F[A] =
      ApplicativeError.liftFromOption(oa, ifEmpty)
  }
}
