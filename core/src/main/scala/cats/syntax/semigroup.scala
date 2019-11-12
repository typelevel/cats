package cats
package syntax

trait SemigroupSyntax {
  // TODO: use simulacrum instances eventually
  implicit final def catsSyntaxSemigroup[A: Semigroup](a: A): SemigroupOps[A] =
    new SemigroupOps[A](a)
}

final class SemigroupOps[A: Semigroup](lhs: A) {
  def |+|(rhs: A): A = Semigroup[A].combine(lhs, rhs)
  def combine(rhs: A): A = Semigroup[A].combine(lhs, rhs)
  def combineN(rhs: Int): A = Semigroup[A].combineN(lhs, rhs)
}
