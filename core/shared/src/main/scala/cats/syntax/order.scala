package cats
package syntax

import cats.macros.Ops

trait OrderSyntax {
  implicit def orderSyntax[A: Order](a: A): OrderOps[A] = new OrderOps[A](a)
}

class OrderOps[A](lhs: A)(implicit A: Order[A]) {
  def compare(rhs: A): Int = macro Ops.binop[A, Int]
  def min(rhs: A): A = macro Ops.binop[A, A]
  def max(rhs: A): A = macro Ops.binop[A, A]
}
