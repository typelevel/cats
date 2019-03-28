package cats
package syntax

trait GroupSyntax extends SemigroupSyntax {
  // TODO: use simulacrum instances eventually
  implicit def catsSyntaxGroup[A: Group](a: A): GroupOps[A] =
    new GroupOps[A](a)
}

final class GroupOps[A: Group](lhs: A) {
  def |-|(rhs: A): A = implicitly[Group[A]].remove(lhs, rhs)
  def remove(rhs: A): A = implicitly[Group[A]].remove(lhs, rhs)
  def inverse(): A = implicitly[Group[A]].inverse(lhs)
}
