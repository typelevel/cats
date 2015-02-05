package cats
package syntax

import cats.macros.Ops
import scala.language.experimental.macros

trait EqSyntax {
  implicit def eqSyntax[A: Eq](a: A) = new EqOps[A](a)
}

class EqOps[A](lhs: A)(implicit A: Eq[A]) {
  def ===(rhs: A): Boolean = macro Ops.binop[A, Boolean]
  def =!=(rhs: A): Boolean = macro Ops.binop[A, Boolean]
}
