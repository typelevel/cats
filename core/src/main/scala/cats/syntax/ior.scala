package cats.syntax

import cats.data.{Ior, IorNel, NonEmptyList}

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
    * res0: IorNel[String, String] = Right(hello)
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
    * res0: IorNel[String, String] = Both(NonEmptyList(error),hello)
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
    * res0: IorNel[String, String] = Both(NonEmptyList(I got it wrong),hello)
    * }}}
    */
  def putLeftIorNel[B](right: B): IorNel[A, B] = Ior.bothNel(a, right)
}


final class IorNelListOps[A, B](val list: List[IorNel[A, B]]) extends AnyVal {

  /**
    * Returns single combined Ior by reducing a list of IorNel
    *
    * Example:
    * {{{
    * scala> import cats.data.Ior
    * scala> import cats.data.NonEmptyList
    * scala> import cats.implicits._
    *
    * scala> List("hello".rightIorNel[String], "error".leftIorNel[String]).reduceToIor
    * res0: Ior[NonEmptyList[String], NonEmptyList[String]] = Both(NonEmptyList(error),NonEmptyList(hello))
    * }}}
    */
  def reduceToIor: Ior[NonEmptyList[A], NonEmptyList[B]] =
    list.map(_.map(NonEmptyList(_, Nil))).reduce(_ combine _)

  /**
    * Returns an Option of a single combined Ior by reducing a list of IorNel
    *
    * Example:
    * {{{
    * scala> import cats.data.Ior
    * scala> import cats.data.IorNel
    * scala> import cats.data.NonEmptyList
    * scala> import cats.implicits._
    *
    * scala> List("hello".rightIorNel[String], "error".leftIorNel[String]).reduceToOptionIor
    * res0: Option[Ior[NonEmptyList[String], NonEmptyList[String]]] = Some(Both(NonEmptyList(error),NonEmptyList(hello)))
    *
    * scala> List.empty[IorNel[String, String]].reduceToOptionIor
    * res1: Option[Ior[NonEmptyList[String], NonEmptyList[String]]] = None
    * }}}
    */
  def reduceToOptionIor: Option[Ior[NonEmptyList[A], NonEmptyList[B]]] =
    list.map(_.map(NonEmptyList(_, Nil))) reduceOption (_ combine _)
}
