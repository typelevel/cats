package cats
package syntax

trait ComonadSyntax {
  // TODO: use simulacrum instances eventually
  implicit def comonadSyntax[F[_]: Comonad, A](fa: F[A]) =
    new ComonadOps[F, A](fa)
}

class ComonadOps[F[_], A](fa: F[A])(implicit F: Comonad[F]) {
  def extract: A = F.extract(fa)
}
