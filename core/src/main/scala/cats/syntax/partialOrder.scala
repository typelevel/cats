package cats
package syntax

trait PartialOrderSyntax extends EqSyntax {
  implicit final def catsSyntaxPartialOrder[A: PartialOrder](a: A): PartialOrderOps[A] =
    new PartialOrderOps[A](a)
}

final class PartialOrderOps[A](lhs: A)(implicit A: PartialOrder[A]) {
  def >(rhs: A): Boolean = A.gt(lhs, rhs)
  def >=(rhs: A): Boolean = A.gteqv(lhs, rhs)
  def <(rhs: A): Boolean = A.lt(lhs, rhs)
  def <=(rhs: A): Boolean = A.lteqv(lhs, rhs)

  def partialCompare(rhs: A): Double = A.partialCompare(lhs, rhs)
  def partialComparison(rhs: A): Option[Comparison] = A.partialComparison(lhs, rhs)
  def tryCompare(rhs: A): Option[Int] = A.tryCompare(lhs, rhs)
  def pmin(rhs: A): Option[A] = A.pmin(lhs, rhs)
  def pmax(rhs: A): Option[A] = A.pmax(lhs, rhs)
}
