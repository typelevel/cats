package cats
package syntax

trait MonoidSyntax extends SemigroupSyntax {
  // TODO: use simulacrum instances eventually
  implicit final def catsSyntaxMonoid[A: Monoid](a: A): MonoidOps[A] =
    new MonoidOps[A](a)
}

final class MonoidOps[A](val lhs: A) extends AnyVal {
  def isEmpty(implicit A: Monoid[A], eq: Eq[A]): Boolean = A.isEmpty(lhs)(eq)
}
