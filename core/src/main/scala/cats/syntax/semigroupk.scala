package cats
package syntax

trait SemigroupKSyntax {
  // TODO: use simulacrum instances eventually
  implicit def semigroupSyntax[FA](a: FA)(implicit U: Unapply[SemigroupK,FA]): SemigroupKOps[U.M, U.A] =
    new SemigroupKOps[U.M, U.A](U.subst(a))(U.TC)
}

class SemigroupKOps[F[_], A](lhs: F[A])(implicit F: SemigroupK[F]) {
  def <+>(rhs: F[A]): F[A] = F.combine(lhs, rhs)
  def combine(rhs: F[A]): F[A] = F.combine(lhs, rhs)
}
