package cats
package syntax

trait FoldableSyntax1 {
  implicit def foldableSyntaxU[FA](fa: FA)(implicit U: Unapply[Foldable,FA]): Foldable.Ops[U.M, U.A] =
    new Foldable.Ops[U.M, U.A] {
      val self = U.subst(fa)
      val typeClassInstance = U.TC
      }
}

trait FoldableSyntax extends Foldable.ToFoldableOps with FoldableSyntax1 {
  implicit def nestedFoldableSyntax[F[_]: Foldable, G[_], A](fga: F[G[A]]): NestedFoldableOps[F, G, A] =
    new NestedFoldableOps[F, G, A](fga)
}

class NestedFoldableOps[F[_], G[_], A](fga: F[G[A]])(implicit F: Foldable[F]) {
  def sequence_[B](implicit G: Applicative[G]): G[Unit] = F.sequence_(fga)
  def foldK(implicit G: MonoidK[G]): G[A] = F.foldK(fga)
}
