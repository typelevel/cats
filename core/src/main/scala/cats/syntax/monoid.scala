package cats
package syntax

trait MonoidSyntax extends SemigroupSyntax {
  // TODO: use simulacrum instances eventually
  implicit def catsSyntaxMonoid[A: Monoid](a: A): MonoidOps[A] =
    new MonoidOps[A](a)
}

final class MonoidOps[A: Monoid](lhs: A) {
  def isEmpty(implicit eq: Eq[A]): Boolean = Monoid[A].isEmpty(lhs)(eq)
}
