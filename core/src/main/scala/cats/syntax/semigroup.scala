package cats
package syntax

trait SemigroupSyntax {
  // TODO: use simulacrum instances eventually
  implicit def catsSyntaxSemigroup[A: Semigroup](a: A): SemigroupOps[A] =
    new SemigroupOps[A](a)
}

final class SemigroupOps[A: Semigroup](lhs: A) {
  def |+|(rhs: A): A = implicitly[Semigroup[A]].combine(lhs, rhs)
  def combine(rhs: A): A = implicitly[Semigroup[A]].combine(lhs, rhs)
  def combineN(rhs: Int): A = implicitly[Semigroup[A]].combineN(lhs, rhs)
}
