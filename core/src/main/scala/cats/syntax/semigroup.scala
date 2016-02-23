package cats
package syntax

import cats.macros.Ops

trait SemigroupSyntax {
  // TODO: use simulacrum instances eventually
  implicit def semigroupSyntax[A: Semigroup](a: A): SemigroupOps[A] =
    new SemigroupOps[A](a)
}

final class SemigroupOps[A: Semigroup](lhs: A) {
  def |+|(rhs: A): A = macro Ops.binop[A, A]
  def combine(rhs: A): A = macro Ops.binop[A, A]
  def combineN(rhs: Int): A = macro Ops.binop[A, A]
}
