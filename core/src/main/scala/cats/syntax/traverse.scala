package cats
package syntax

trait TraverseSyntax1 {
  implicit def traverseSyntaxU[FA](fa: FA)(implicit U: Unapply[Traverse,FA]): TraverseOps[U.M, U.A] =
    new TraverseOps(U.subst(fa))(U.TC)
}

trait TraverseSyntax extends TraverseSyntax1 {
  // TODO: use simulacrum instances eventually
  implicit def traverseSyntax[F[_]: Traverse, A](fa: F[A]): TraverseOps[F, A] =
    new TraverseOps(fa)

  implicit def nestedTraverseSyntax[F[_]: Traverse, G[_], A](fga: F[G[A]]): NestedTraverseOps[F, G, A] =
    new NestedTraverseOps[F, G, A](fga)
}

final class TraverseOps[F[_], A](fa: F[A])(implicit F: Traverse[F]) {
  def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] = F.traverse(fa)(f)

  def traverseU[GB](f: A => GB)(implicit U: Unapply[Applicative, GB]): U.M[F[U.A]] =
    F.traverseU[A, GB](fa)(f)(U)

  def sequence[G[_], B](implicit G: Applicative[G], ev: A =:= G[B]): G[F[B]] =
    F.sequence(fa.asInstanceOf[F[G[B]]])

  def sequenceU(implicit U: Unapply[Applicative,A]): U.M[F[U.A]] =
    F.sequenceU[A](fa)(U)

}

final class NestedTraverseOps[F[_], G[_], A](fga: F[G[A]])(implicit F: Traverse[F]) {
  def sequence(implicit G: Applicative[G]): G[F[A]] = F.sequence(fga)
}
