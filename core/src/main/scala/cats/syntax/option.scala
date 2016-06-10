package cats
package syntax

import cats.data.{ Xor, Validated, ValidatedNel }

trait OptionSyntax {
  final def none[A]: Option[A] = Option.empty[A]
  implicit final def optionIdSyntax[A](a: A): OptionIdOps[A] = new OptionIdOps(a)
  implicit final def optionSyntax[A](oa: Option[A]): OptionOps[A] = new OptionOps(oa)
}

final class OptionIdOps[A](val a: A) extends AnyVal {
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

final class OptionOps[A](val oa: Option[A]) extends AnyVal {
  /**
   * If the `Option` is a `Some`, return its value in a [[cats.data.Xor.Left]].
   * If the `Option` is `None`, return the provided `B` value in a
   * [[cats.data.Xor.Right]].
   *
   * Example:
   * {{{
   * scala> import cats.data.Xor
   * scala> import cats.implicits._
   *
   * scala> val error1: Option[String] = Some("error!")
   * scala> error1.toLeftXor(3)
   * res0: String Xor Int = Left(error!)
   *
   * scala> val error2: Option[String] = None
   * scala> error2.toLeftXor(3)
   * res1: String Xor Int = Right(3)
   * }}}
   */
  def toLeftXor[B](b: => B): A Xor B = oa.fold[A Xor B](Xor.Right(b))(Xor.Left(_))

  /**
   * If the `Option` is a `Some`, return its value in a [[cats.data.Xor.Right]].
   * If the `Option` is `None`, return the provided `B` value in a
   * [[cats.data.Xor.Left]].
   *
   * Example:
   * {{{
   * scala> import cats.data.Xor
   * scala> import cats.implicits._
   *
   * scala> val result1: Option[Int] = Some(3)
   * scala> result1.toRightXor("error!")
   * res0: String Xor Int = Right(3)
   *
   * scala> val result2: Option[Int] = None
   * scala> result2.toRightXor("error!")
   * res1: String Xor Int = Left(error!)
   * }}}
   */
  def toRightXor[B](b: => B): B Xor A = oa.fold[B Xor A](Xor.Left(b))(Xor.Right(_))

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
   * res0: ValidatedNel[String, Int] = Invalid(OneAnd(error!,List()))
   *
   * scala> val error2: Option[String] = None
   * scala> error2.toInvalidNel(3)
   * res1: ValidatedNel[String, Int] = Valid(3)
   * }}}
   */
  def toInvalidNel[B](b: => B): ValidatedNel[A, B] = oa.fold[ValidatedNel[A, B]](Validated.Valid(b))(Validated.invalidNel(_))

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
   * res1: ValidatedNel[String, Int] = Invalid(OneAnd(error!,List()))
   * }}}
   */
  def toValidNel[B](b: => B): ValidatedNel[B, A] = oa.fold[ValidatedNel[B, A]](Validated.invalidNel(b))(Validated.Valid(_))

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
}
