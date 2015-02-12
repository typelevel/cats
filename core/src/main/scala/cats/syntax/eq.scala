package cats
package syntax

import cats.macros.Ops

trait EqSyntax {
  implicit def eqSyntax[A: Eq](a: A): EqOps[A] = new EqOps[A](a)
}

class EqOps[A](lhs: A)(implicit A: Eq[A]) {
  def ===(rhs: A): Boolean = macro Ops.binop[A, Boolean]
  def =!=(rhs: A): Boolean = macro Ops.binop[A, Boolean]
}
