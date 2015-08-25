package cats
package syntax

import cats.data.Xor

trait XorSyntax {
  implicit def xorSyntax[A](a: A): XorOps[A] = new XorOps(a)
}

class XorOps[A](a: A) {
  def left[B]: A Xor B = Xor.Left(a)
  def right[B]: B Xor A = Xor.Right(a)
}
