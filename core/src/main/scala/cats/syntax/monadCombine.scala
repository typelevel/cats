package cats
package syntax

trait MonadCombineSyntax {
  // TODO: use simulacrum instances eventually
  implicit def nestedMonadCombineSyntax[F[_]: MonadCombine, G[_], A](fga: F[G[A]]): NestedMonadCombineOps[F, G, A] =
    new NestedMonadCombineOps[F, G, A](fga)
}

final class NestedMonadCombineOps[F[_], G[_], A](fga: F[G[A]])(implicit F: MonadCombine[F]) {
  def unite(implicit G: Foldable[G]): F[A] = F.unite(fga)
}
