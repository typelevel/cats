package cats
package syntax

import cats.macros.Ops

trait EqSyntax {
  implicit def catsSyntaxEq[A: Eq](a: A): EqOps[A] =
    new EqOps[A](a)
}

final class EqOps[A: Eq](lhs: A) {
  def ===(rhs: A): Boolean = macro Ops.binop[A, Boolean]
  def =!=(rhs: A): Boolean = macro Ops.binop[A, Boolean]
}
