package cats
package syntax

trait ComonadSyntax1 {
  implicit def comonadSyntaxU[FA](fa: FA)(implicit U: Unapply[Comonad, FA]): ComonadOps[U.M, U.A] =
    new ComonadOps(U.subst(fa))(U.TC)
}

trait ComonadSyntax extends ComonadSyntax1 {
  implicit def comonadSyntax[F[_]: Comonad, A](fa: F[A]): ComonadOps[F, A] =
    new ComonadOps(fa)
}

class ComonadOps[F[_], A](fa: F[A])(implicit F: Comonad[F]) {
  def extract: A = F.extract(fa)
}
