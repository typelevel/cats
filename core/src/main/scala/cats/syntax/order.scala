package cats
package syntax

import cats.macros.Ops
import cats.kernel.Comparison

trait OrderSyntax extends PartialOrderSyntax {
  implicit final def catsSyntaxOrder[A: Order](a: A): OrderOps[A] =
    new OrderOps[A](a)
}

final class OrderOps[A: Order](lhs: A) {
  def compare(rhs: A): Int = macro Ops.binop[A, Int]
  def min(rhs: A): A = macro Ops.binop[A, A]
  def max(rhs: A): A = macro Ops.binop[A, A]
  def comparison(rhs: A): Comparison = macro Ops.binop[A, Comparison]
}
