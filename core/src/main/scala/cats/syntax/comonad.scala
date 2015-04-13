package cats
package syntax

trait ComonadSyntax {
  // TODO: use simulacrum instances eventually
  implicit def comonadSyntax[FA](fa: FA)(implicit U: Unapply[Comonad, FA]): ComonadOps[U.M, U.A] =
    new ComonadOps[U.M, U.A](U.subst(fa))(U.TC)
}

class ComonadOps[F[_], A](fa: F[A])(implicit F: Comonad[F]) {
  def extract: A = F.extract(fa)
}
