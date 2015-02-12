package cats
package syntax

trait FoldableSyntax {
  // TODO: use simulacrum instances eventually
  implicit def foldableSyntax[F[_]: Foldable, A](fa: F[A]): FoldableOps[F, A] =
    new FoldableOps[F, A](fa)

  implicit def nestedFoldableSyntax[F[_]: Foldable, G[_], A](fga: F[G[A]]): NestedFoldableOps[F, G, A] =
    new NestedFoldableOps[F, G, A](fga)
}

class FoldableOps[F[_], A](fa: F[A])(implicit F: Foldable[F]) {
  def foldLeft[B](b: B)(f: (B, A) => B): B = F.foldLeft(fa, b)(f)
  def foldRight[B](b: B)(f: (A, B) => B): B = F.foldRight(fa, b)(f)
  def foldLazy[B](b: Lazy[B])(f: A => Fold[B]): Lazy[B] = F.foldLazy(fa, b)(f)
  def foldMap[B: Monoid](f: A => B): B = F.foldMap(fa)(f)
  def fold(implicit A: Monoid[A]): A = F.fold(fa)
  def traverse_[G[_]: Applicative, B](f: A => G[B]): G[Unit] = F.traverse_(fa)(f)
}

class NestedFoldableOps[F[_], G[_], A](fga: F[G[A]])(implicit F: Foldable[F]) {
  def sequence_[B](implicit G: Applicative[G]): G[Unit] = F.sequence_(fga)
  def foldK(fga: F[G[A]])(implicit G: MonoidK[G]): G[A] = F.foldK(fga)
}
