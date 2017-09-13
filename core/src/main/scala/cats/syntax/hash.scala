package cats
package syntax

import cats.macros.Ops

trait HashSyntax {

  implicit def catsSyntaxHash[A: Hash](a: A): HashOps[A] =
    new HashOps[A](a)

}

final class HashOps[A: Hash](a: A) {
  def hash: Int = macro Ops.unop[Int]
}
