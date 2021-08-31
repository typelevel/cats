package cats
package syntax

trait GroupSyntax extends SemigroupSyntax {
  // TODO: use simulacrum instances eventually
  implicit final def catsSyntaxGroup[A: Group](a: A): GroupOps[A] =
    new GroupOps[A](a)
}

final class GroupOps[A: Group](lhs: A) {
  def |-|(rhs: A): A = Group[A].remove(lhs, rhs)
  def remove(rhs: A): A = Group[A].remove(lhs, rhs)
  def inverse(): A = Group[A].inverse(lhs)
}
