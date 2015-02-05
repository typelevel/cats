package cats
package syntax

trait MonadFilterSyntax {
  // TODO: use simulacrum instances eventually
  implicit def monadFilterSyntax[F[_]: MonadFilter, A](fa: F[A]) =
    new MonadFilterOps[F, A](fa)
}

class MonadFilterOps[F[_], A](fa: F[A])(implicit F: MonadFilter[F]) {
  def filter(f: A => Boolean): F[A] = F.filter(fa)(f)
  def filterM(f: A => F[Boolean]): F[A] = F.filterM(fa)(f)
}
