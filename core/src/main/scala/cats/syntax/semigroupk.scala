package cats
package syntax

trait SemigroupKSyntax {
  // TODO: use simulacrum instances eventually
  implicit def semigroupSyntax[F[_]: SemigroupK, A](a: F[A]): SemigroupKOps[F, A] =
    new SemigroupKOps[F, A](a)
}

class SemigroupKOps[F[_], A](lhs: F[A])(implicit F: SemigroupK[F]) {
  def <+>(rhs: F[A]): F[A] = F.combine(lhs, rhs)
  def combine(rhs: F[A]): F[A] = F.combine(lhs, rhs)
}
