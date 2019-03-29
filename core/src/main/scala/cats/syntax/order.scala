package cats
package syntax

import cats.kernel.Comparison

trait OrderSyntax extends PartialOrderSyntax {
  implicit final def catsSyntaxOrder[A: Order](a: A): OrderOps[A] =
    new OrderOps[A](a)
}

final class OrderOps[A: Order](lhs: A) {
  def compare(rhs: A): Int = implicitly[Order[A]].compare(lhs, rhs)
  def min(rhs: A): A = implicitly[Order[A]].min(lhs, rhs)
  def max(rhs: A): A = implicitly[Order[A]].max(lhs, rhs)
  def comparison(rhs: A): Comparison = implicitly[Order[A]].comparison(lhs, rhs)
}
