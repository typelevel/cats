package cats
package syntax

trait MonadFilterSyntax1 {
  implicit def monadFilterSyntaxU[FA](fa: FA)(implicit U: Unapply[MonadFilter,FA]): MonadFilterOps[U.M, U.A] =
    new MonadFilterOps[U.M, U.A](U.subst(fa))(U.TC)
}

trait MonadFilterSyntax extends MonadFilterSyntax1 {
  implicit def monadFilterSyntax[F[_]: MonadFilter, A](fa: F[A]): MonadFilterOps[F, A] =
    new MonadFilterOps(fa)
}

class MonadFilterOps[F[_], A](fa: F[A])(implicit F: MonadFilter[F]) {
  def filter(f: A => Boolean): F[A] = F.filter(fa)(f)
  def filterM(f: A => F[Boolean]): F[A] = F.filterM(fa)(f)
}
