package cats
package syntax

import cats.macros.Ops

trait EqSyntax {
  /** not final so it can be disabled in favor of scalactic equality in tests */
  implicit def catsSyntaxEq[A: Eq](a: A): EqOps[A] =
    new EqOps[A](a)
}

final class EqOps[A: Eq](lhs: A) {
  def ===(rhs: A): Boolean  = macro Ops.binop[A, Boolean]
  def =!=(rhs: A): Boolean  = macro Ops.binop[A, Boolean]
  def eqv(rhs: A): Boolean  = Eq[A].eqv(lhs,rhs)
  def neqv(rhs: A): Boolean = Eq[A].neqv(lhs,rhs)
}
