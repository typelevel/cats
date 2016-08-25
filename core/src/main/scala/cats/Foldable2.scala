package cats

import simulacrum.typeclass

/**
 * A type class abstracting over types that give rise to two independent [[cats.Foldable]]s.
 */
@typeclass trait Foldable2[F[_, _]] { self =>
  /** Collapse the structure with a left-associative function */
  def fold2Left[A, B, C](fab: F[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C

  /** Collapse the structure with a right-associative function */
  def fold2Right[A, B, C](fab: F[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C]

  /** Collapse the structure by mapping each element to an element of a type that has a [[cats.Monoid]] */
  def fold2Map[A, B, C](fab: F[A, B])(f: A => C, g: B => C)(implicit C: Monoid[C]): C =
    fold2Left(fab, C.empty)(
      (c: C, a: A) => C.combine(c, f(a)),
      (c: C, b: B) => C.combine(c, g(b))
    )

  def compose[G[_, _]](implicit ev: Foldable2[G]): Foldable2[λ[(α, β) => F[G[α, β], G[α, β]]]] =
    new ComposedFoldable2[F, G] {
      val F = self
      val G = ev
    }
}

private[cats] trait ComposedFoldable2[F[_, _], G[_, _]] extends Foldable2[λ[(α, β) => F[G[α, β], G[α, β]]]] {
  implicit def F: Foldable2[F]
  implicit def G: Foldable2[G]

  override def fold2Left[A, B, C](fab: F[G[A, B], G[A, B]], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    F.fold2Left(fab, c)(
      (c: C, gab: G[A, B]) => G.fold2Left(gab, c)(f, g),
      (c: C, gab: G[A, B]) => G.fold2Left(gab, c)(f, g)
    )

  override def fold2Right[A, B, C](fab: F[G[A, B], G[A, B]], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
    F.fold2Right(fab, c)(
      (gab: G[A, B], c: Eval[C]) => G.fold2Right(gab, c)(f, g),
      (gab: G[A, B], c: Eval[C]) => G.fold2Right(gab, c)(f, g)
    )
}
