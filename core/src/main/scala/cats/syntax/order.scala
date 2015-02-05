package cats
package syntax

trait OrderSyntax {
  // TODO: use simulacrum instances eventually
  implicit def orderSyntax[A: Order](a: A) =
    new OrderOps[A](a)
}

class OrderOps[A](lhs: A)(implicit A: Order[A]) {
  def compare(rhs: A): Int = A.compare(lhs, rhs)
  def min(rhs: A): A = A.min(lhs, rhs)
  def max(rhs: A): A = A.max(lhs, rhs)
}
