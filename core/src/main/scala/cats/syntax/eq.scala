package cats
package syntax

trait EqSyntax {
  // TODO: use simulacrum instances eventually
  implicit def eqSyntax[A: Eq](a: A) =
    new EqOps[A](a)
}

class EqOps[A](lhs: A)(implicit A: Eq[A]) {
  def ===(rhs: A): Boolean = A.eqv(lhs, rhs)
  def =!=(rhs: A): Boolean = A.neqv(lhs, rhs)
}
