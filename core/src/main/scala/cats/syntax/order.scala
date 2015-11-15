package cats
package syntax

import cats.macros.Ops

trait OrderSyntax extends PartialOrderSyntax {
  implicit def orderSyntax[A: Order](a: A): OrderOps[A] =
    new OrderOps[A](a)
}

final class OrderOps[A: Order](lhs: A) {
  def compare(rhs: A): Int = macro Ops.binop[A, Int]
  def min(rhs: A): A = macro Ops.binop[A, A]
  def max(rhs: A): A = macro Ops.binop[A, A]
}
