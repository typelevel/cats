package cats
package syntax

import cats.macros.Ops

trait GroupSyntax extends SemigroupSyntax {
  // TODO: use simulacrum instances eventually
  implicit final def catsSyntaxGroup[A: Group](a: A): GroupOps[A] =
    new GroupOps[A](a)
}

final class GroupOps[A: Group](lhs: A) {
  def |-|(rhs: A): A = macro Ops.binop[A, A]
  def remove(rhs: A): A = macro Ops.binop[A, A]
  def inverse(): A = macro Ops.unop[A]
}
