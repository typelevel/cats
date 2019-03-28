package cats
package syntax

trait EqSyntax {
  implicit def catsSyntaxEq[A: Eq](a: A): EqOps[A] =
    new EqOps[A](a)
}

final class EqOps[A: Eq](lhs: A) {
  def ===(rhs: A): Boolean = implicitly[Eq[A]].eqv(lhs, rhs)
  def =!=(rhs: A): Boolean = implicitly[Eq[A]].neqv(lhs, rhs)
}
