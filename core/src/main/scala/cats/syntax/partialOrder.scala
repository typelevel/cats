package cats
package syntax

trait PartialOrderSyntax {
  // TODO: use simulacrum instances eventually
  implicit def partialOrderSyntax[A: PartialOrder](a: A) =
    new PartialOrderOps[A](a)
}

class PartialOrderOps[A](lhs: A)(implicit A: PartialOrder[A]) {
  def >(rhs: A): Boolean = A.gt(lhs, rhs)
  def >=(rhs: A): Boolean = A.gteqv(lhs, rhs)
  def <(rhs: A): Boolean = A.lt(lhs, rhs)
  def <=(rhs: A): Boolean = A.lteqv(lhs, rhs)

  def partialCompare(rhs: A): Double = A.partialCompare(lhs, rhs)
  def tryCompare(rhs: A): Option[Int] = A.tryCompare(lhs, rhs)
  def pmin(rhs: A): Option[A] = A.pmin(lhs, rhs)
  def pmax(rhs: A): Option[A] = A.pmax(lhs, rhs)
}
