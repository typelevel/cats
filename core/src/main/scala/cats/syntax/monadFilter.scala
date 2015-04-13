package cats
package syntax

trait MonadFilterSyntax {
  // TODO: use simulacrum instances eventually
  implicit def monadFilterSyntax[FA](fa: FA)(implicit U: Unapply[MonadFilter,FA]): MonadFilterOps[U.M, U.A] =
    new MonadFilterOps[U.M, U.A](U.subst(fa))(U.TC)
}

class MonadFilterOps[F[_], A](fa: F[A])(implicit F: MonadFilter[F]) {
  def filter(f: A => Boolean): F[A] = F.filter(fa)(f)
  def filterM(f: A => F[Boolean]): F[A] = F.filterM(fa)(f)
}
