package cats
package syntax

import cats.kernel.Comparison

trait OrderSyntax extends PartialOrderSyntax {
  implicit final def catsSyntaxOrder[A: Order](a: A): OrderOps[A] =
    new OrderOps[A](a)
}

final class OrderOps[A: Order](lhs: A) {
  def compare(rhs: A): Int = Order[A].compare(lhs, rhs)
  def min(rhs: A): A = Order[A].min(lhs, rhs)
  def max(rhs: A): A = Order[A].max(lhs, rhs)
  def comparison(rhs: A): Comparison = Order[A].comparison(lhs, rhs)
}
