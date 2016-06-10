package cats
package syntax

import cats.data.Xor

trait EitherSyntax {
  implicit def catsSyntaxEither[A, B](eab: Either[A, B]): EitherOps[A, B] = new EitherOps(eab)
}

final class EitherOps[A, B](val eab: Either[A, B]) extends AnyVal {

  /**
   * Convert a `scala.util.Either` into a [[cats.data.Xor]].
   *
   * Example:
   * {{{
   * scala> import cats.data.Xor
   * scala> import cats.implicits._
   *
   * scala> val i: Either[String, Int] = Right(3)
   * scala> i.toXor
   * res0: Xor[String, Int] = Right(3)
   *
   * scala> val s: Either[String, Int] = Left("error!")
   * scala> s.toXor
   * res0: Xor[String, Int] = Left(error!)
   * }}}
   */
  def toXor: A Xor B = Xor.fromEither(eab)
}
