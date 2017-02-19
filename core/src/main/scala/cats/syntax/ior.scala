package cats.syntax

import cats.data.{Ior, IorNel}

trait IorSyntax {
  implicit def catsSyntaxIorId[A](a: A): IorIdOps[A] = new IorIdOps(a)
  implicit def catsSyntaxIorId[A, B](list: List[IorNel[A, B]]): IorNelListOps[A, B] =
    new IorNelListOps(list)
}

final class IorIdOps[A](val a: A) extends AnyVal {
  /**
    * Wrap a value in `Ior.Right`.
    *
    * Example:
    * {{{
    * scala> import cats.implicits._
    * scala> "hello".rightIor[String]
    * res0: Ior[String, String] = Ior.Right("hello")
    * }}}
    */
  def rightIor[B]: Ior[B, A] = Ior.right(a)

  /**
    * Wrap a value in `Ior.Left`.
    *
    * Example:
    * {{{
    * scala> import cats.implicits._
    * scala> "hello".leftIor[String]
    * res0: Ior[String, String] = Ior.Left("hello")
    * }}}
    */
  def leftIor[B]: Ior[A, B] = Ior.left(a)

  /**
    * Wrap a value in the right side of `Ior.Both`.
    *
    * Example:
    * {{{
    * scala> import cats.implicits._
    * scala> "hello".putRightIor[String]("error")
    * res0: Ior[String, String] = Ior.Both("error", "hello")
    * }}}
    */
  def putRightIor[B](left: B): Ior[B, A] = Ior.both(left, a)

  /**
    * Wrap a value in the left side of `Ior.Both`.
    *
    * Example:
    * {{{
    * scala> import cats.implicits._
    * scala> "I got it wrong".putLeftIor[String]("hello")
    * res0: Ior[String, String] = Ior.Both("I got it wrong", "hello")
    * }}}
    */
  def putLeftIor[B](right: B): Ior[A, B] = Ior.both(a, right)

  /**
    * Wrap a value in a NonEmptyList in `Ior.Right`.
    *
    * Example:
    * {{{
    * scala> import cats.implicits._
    * scala> "hello".rightNelIor[String]
    * res0: IorNel[String, String] = Ior.Right(NonEmptyList("hello", Nil))
    * }}}
    */
  def rightNelIor[B]: IorNel[B, A] = Ior.rightNel(a)

  /**
    * Wrap a value in a NonEmptyList in `Ior.Left`.
    *
    * Example:
    * {{{
    * scala> import cats.implicits._
    * scala> "hello".leftNelIor[String]
    * res0: IorNel[String, String] = Ior.Left(NonEmptyList("hello", Nil))
    * }}}
    */
  def leftNelIor[B]: IorNel[A, B] = Ior.leftNel(a)

  /**
    * Wrap a value in a NonEmptyList in the right side of `Ior.Both`.
    *
    * Example:
    * {{{
    * scala> import cats.implicits._
    * scala> "hello".putRightNelIor[String]("error")
    * res0: IorNel[String, String] = Ior.Both(NonEmptyList("error", Nil), NonEmptyList("hello", Nil))
    * }}}
    */
  def putRightNelIor[B](left: B): IorNel[B, A] = Ior.bothNel(left, a)

  /**
    * Wrap a value in a NonEmptyList in the left side of `Ior.Both`.
    *
    * Example:
    * {{{
    * scala> import cats.implicits._
    * scala> "I got it wrong".putLeftNelIor[String]("hello")
    * res0: IorNel[String, String] = Ior.Both(NonEmptyList("I got it wrong", Nil), NonEmptyList("hello", Nil))
    * }}}
    */
  def putLeftNelIor[B](right: B): IorNel[A, B] = Ior.bothNel(a, right)
}


final class IorNelListOps[A, B](val list: List[IorNel[A, B]]) extends AnyVal {

  def reduceIorNel: IorNel[A, B] = list reduce (_ append _)

  def reduceOptionIorNel: Option[IorNel[A, B]] = list reduceOption (_ append _)
}
