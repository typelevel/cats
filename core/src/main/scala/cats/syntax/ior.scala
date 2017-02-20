package cats.syntax

import cats.data.{Ior, IorNel}

trait IorSyntax {
  implicit def catsSyntaxIorId[A](a: A): IorIdOps[A] = new IorIdOps(a)
  implicit def catsSyntaxListIorNel[A, B](list: List[IorNel[A, B]]): IorNelListOps[A, B] =
    new IorNelListOps(list)
}

final class IorIdOps[A](val a: A) extends AnyVal {
  /**
    * Wrap a value in `Ior.Right`.
    *
    * Example:
    * {{{
    * scala> import cats.data.Ior
    * scala> import cats.implicits._
    *
    * scala> "hello".rightIor[String]
    * res0: Ior[String, String] = Right(hello)
    * }}}
    */
  def rightIor[B]: Ior[B, A] = Ior.right(a)

  /**
    * Wrap a value in `Ior.Left`.
    *
    * Example:
    * {{{
    * scala> import cats.data.Ior
    * scala> import cats.implicits._
    *
    * scala> "error".leftIor[String]
    * res0: Ior[String, String] = Left(error)
    * }}}
    */
  def leftIor[B]: Ior[A, B] = Ior.left(a)

  /**
    * Wrap a value in the right side of `Ior.Both`.
    *
    * Example:
    * {{{
    * scala> import cats.data.Ior
    * scala> import cats.implicits._
    *
    * scala> "hello".putRightIor("error")
    * res0: Ior[String, String] = Both(error,hello)
    * }}}
    */
  def putRightIor[B](left: B): Ior[B, A] = Ior.both(left, a)

  /**
    * Wrap a value in the left side of `Ior.Both`.
    *
    * Example:
    * {{{
    * scala> import cats.data.Ior
    * scala> import cats.implicits._
    *
    * scala> "error".putLeftIor("hello")
    * res0: Ior[String, String] = Both(error,hello)
    * }}}
    */
  def putLeftIor[B](right: B): Ior[A, B] = Ior.both(a, right)

  /**
    * Wrap a value in a NonEmptyList in `Ior.Right`.
    *
    * Example:
    * {{{
    * scala> import cats.data.IorNel
    * scala> import cats.implicits._
    *
    * scala> "hello".rightIorNel[String]
    * res0: IorNel[String, String] = Right(NonEmptyList(hello))
    * }}}
    */
  def rightIorNel[B]: IorNel[B, A] = Ior.rightNel(a)

  /**
    * Wrap a value in a NonEmptyList in `Ior.Left`.
    *
    * Example:
    * {{{
    * scala> import cats.data.IorNel
    * scala> import cats.implicits._
    *
    * scala> "error".leftIorNel[String]
    * res0: IorNel[String, String] = Left(NonEmptyList(error))
    * }}}
    */
  def leftIorNel[B]: IorNel[A, B] = Ior.leftNel(a)

  /**
    * Wrap a value in a NonEmptyList in the right side of `Ior.Both`.
    *
    * Example:
    * {{{
    * scala> import cats.data.IorNel
    * scala> import cats.implicits._
    *
    * scala> "hello".putRightIorNel[String]("error")
    * res0: IorNel[String, String] = Both(NonEmptyList(error),NonEmptyList(hello))
    * }}}
    */
  def putRightIorNel[B](left: B): IorNel[B, A] = Ior.bothNel(left, a)

  /**
    * Wrap a value in a NonEmptyList in the left side of `Ior.Both`.
    *
    * Example:
    * {{{
    * scala> import cats.data.IorNel
    * scala> import cats.implicits._
    *
    * scala> "I got it wrong".putLeftIorNel[String]("hello")
    * res0: IorNel[String, String] = Both(NonEmptyList(I got it wrong),NonEmptyList(hello))
    * }}}
    */
  def putLeftIorNel[B](right: B): IorNel[A, B] = Ior.bothNel(a, right)
}


final class IorNelListOps[A, B](val list: List[IorNel[A, B]]) extends AnyVal {

  /**
    * Returns single combined IorNel by reducing a list of IorNel
    *
    * Example:
    * {{{
    * scala> import cats.data.IorNel
    * scala> import cats.implicits._
    *
    * scala> List("hello".rightIorNel[String], "error".leftIorNel[String]).reduceToIorNel
    * res0: IorNel[String, String] = Both(NonEmptyList(error),NonEmptyList(hello))
    * }}}
    */
  def reduceToIorNel: IorNel[A, B] = list reduce (_ append _)

  /**
    * Returns an Option of a single combined IorNel by reducing a list of IorNel
    *
    * Example:
    * {{{
    * scala> import cats.data.IorNel
    * scala> import cats.implicits._
    *
    * scala> List("hello".rightIorNel[String], "error".leftIorNel[String]).reduceToOptionIorNel
    * res0: Option[IorNel[String, String]] = Some(Both(NonEmptyList(error),NonEmptyList(hello)))
    *
    * scala> List.empty[IorNel[String, String]].reduceToOptionIorNel
    * res1: Option[IorNel[String, String]] = None
    * }}}
    */
  def reduceToOptionIorNel: Option[IorNel[A, B]] = list reduceOption (_ append _)
}
