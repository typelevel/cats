package cats
package syntax

import cats.data.Xor

trait XorSyntax {
  implicit def xorIdSyntax[A](a: A): XorIdOps[A] = new XorIdOps(a)
}

class XorIdOps[A](val a: A) extends AnyVal {
  def left[B]: A Xor B = Xor.Left(a)
  def right[B]: B Xor A = Xor.Right(a)
}
