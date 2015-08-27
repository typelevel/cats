package cats
package syntax

import cats.data.Xor

trait EitherSyntax {
  implicit def eitherSyntax[A, B](a: Either[A, B]): EitherOps[A, B] = new EitherOps(a)
}

class EitherOps[A, B](val a: Either[A, B]) extends AnyVal {
  def toXor: A Xor B = Xor.fromEither(a)
}
