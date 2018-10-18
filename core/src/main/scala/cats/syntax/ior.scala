package cats.syntax

import cats.data.Ior

trait IorSyntax {
  implicit final def catsSyntaxIorId[A](a: A): IorIdOps[A] = new IorIdOps(a)
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
}
