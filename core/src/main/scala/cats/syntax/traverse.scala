package cats
package syntax

trait TraverseSyntax {
  // TODO: use simulacrum instances eventually
  implicit def traverseSyntax[FA](fa: FA)(implicit U: Unapply[Traverse,FA]): TraverseOps[U.M, U.A] =
    new TraverseOps[U.M, U.A](U.subst(fa))(U.TC)

  implicit def nestedTraverseSyntax[F[_]: Traverse, G[_], A](fga: F[G[A]]): NestedTraverseOps[F, G, A] =
    new NestedTraverseOps[F, G, A](fga)
}

class TraverseOps[F[_], A](fa: F[A])(implicit F: Traverse[F]) {
  def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] = F.traverse(fa)(f)

  def traverseU[GB](f: A => GB)(implicit U: Unapply[Applicative, GB]): U.M[F[U.A]] =
    F.traverseU[A, GB](fa)(f)(U)

  def sequenceU(implicit U: Unapply[Applicative,A]): U.M[F[U.A]] =
    F.sequenceU[A](fa)(U)

}

class NestedTraverseOps[F[_], G[_], A](fga: F[G[A]])(implicit F: Traverse[F]) {
  def sequence(implicit G: Applicative[G]): G[F[A]] = F.sequence(fga)
}
