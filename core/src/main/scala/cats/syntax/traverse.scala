package cats
package syntax

trait TraverseSyntax {
  // TODO: use simulacrum instances eventually
  implicit def traverseSyntax[F[_]: Traverse, A](fa: F[A]): TraverseOps[F, A] =
    new TraverseOps[F, A](fa)

  implicit def nestedTraverseSyntax[F[_]: Traverse, G[_], A](fga: F[G[A]]): NestedTraverseOps[F, G, A] =
    new NestedTraverseOps[F, G, A](fga)
}

class TraverseOps[F[_], A](fa: F[A])(implicit F: Traverse[F]) {
  def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] = F.traverse(fa)(f)
}

class NestedTraverseOps[F[_], G[_], A](fga: F[G[A]])(implicit F: Traverse[F]) {
  def sequence(implicit G: Applicative[G]): G[F[A]] = F.sequence(fga)
}
