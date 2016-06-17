package cats
package syntax

trait BifoldableSyntax {
  implicit def catsSyntaxBifoldable[F[_, _]: Bifoldable, A, B](fab: F[A, B]): BifoldableOps[F, A, B] =
    new BifoldableOps[F, A, B](fab)
}

final class BifoldableOps[F[_, _], A, B](fab: F[A, B])(implicit F: Bifoldable[F]) {
  def bifoldLeft[C](c: C)(f: (C, A) => C, g: (C, B) => C): C =
    F.bifoldLeft(fab, c)(f, g)

  def bifoldRight[C](c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
    F.bifoldRight(fab, c)(f, g)

  def bifoldMap[C](f: A => C, g: B => C)(implicit C: Monoid[C]): C =
    F.bifoldMap(fab)(f, g)
}
