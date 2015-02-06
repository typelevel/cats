package cats
package syntax

trait SemigroupSyntax {
  // TODO: use simulacrum instances eventually
  implicit def semigroupSyntax[A: Semigroup](a: A): SemigroupOps[A] =
    new SemigroupOps[A](a)
}

class SemigroupOps[A](lhs: A)(implicit A: Semigroup[A]) {
  def |+|(rhs: A): A = A.combine(lhs, rhs)
  def combine(rhs: A): A = A.combine(lhs, rhs)
  def combineN(rhs: Int): A = A.combineN(lhs, rhs)
}
