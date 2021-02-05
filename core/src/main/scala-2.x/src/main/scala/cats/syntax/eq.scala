package cats
package syntax

import scala.{specialized => sp}

trait EqSyntax {

  /**
   * not final so it can be disabled in favor of scalactic equality in tests
   */
  implicit def catsSyntaxEq[@sp(Int, Long, Float, Double) A: Eq](a: A): EqOps[A] =
    new EqOps[A](a)
}

final case class EqOps[@sp(Int, Long, Float, Double) A: Eq](lhs: A) {
  def ===(rhs: A): Boolean = Eq[A].eqv(lhs, rhs)
  def =!=(rhs: A): Boolean = Eq[A].neqv(lhs, rhs)
  def eqv(rhs: A): Boolean = Eq[A].eqv(lhs, rhs)
  def neqv(rhs: A): Boolean = Eq[A].neqv(lhs, rhs)
}
