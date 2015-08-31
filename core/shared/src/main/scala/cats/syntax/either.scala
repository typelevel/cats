package cats
package syntax

import cats.data.Xor

trait EitherSyntax {
  implicit def eitherSyntax[A, B](eab: Either[A, B]): EitherOps[A, B] = new EitherOps(eab)
}

class EitherOps[A, B](val eab: Either[A, B]) extends AnyVal {
  def toXor: A Xor B = Xor.fromEither(eab)
}
