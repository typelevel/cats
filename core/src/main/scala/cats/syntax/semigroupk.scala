package cats
package syntax

trait SemigroupKSyntax1 {
  // TODO: use simulacrum instances eventually
  implicit def semigroupSyntaxU[FA](a: FA)(implicit U: Unapply[SemigroupK,FA]): SemigroupKOps[U.M, U.A] =
    new SemigroupKOps(U.subst(a))(U.TC)
}

trait SemigroupKSyntax extends SemigroupKSyntax1 {
  // TODO: use simulacrum instances eventually
  implicit def semigroupSyntax[F[_]: SemigroupK, A](a: F[A]): SemigroupKOps[F, A] =
    new SemigroupKOps(a)
}

class SemigroupKOps[F[_], A](lhs: F[A])(implicit F: SemigroupK[F]) {
  def <+>(rhs: F[A]): F[A] = F.combine(lhs, rhs)
  def combine(rhs: F[A]): F[A] = F.combine(lhs, rhs)
}
