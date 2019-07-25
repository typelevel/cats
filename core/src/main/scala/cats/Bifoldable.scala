package cats

import simulacrum.typeclass
import cats.instances.tuple.catsKernelStdMonoidForTuple2

/**
 * A type class abstracting over types that give rise to two independent [[cats.Foldable]]s.
 */
@typeclass trait Bifoldable[F[_, _]] { self =>

  /** Collapse the structure with a left-associative function */
  def bifoldLeft[A, B, C](fab: F[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C

  /** Collapse the structure with a right-associative function */
  def bifoldRight[A, B, C](fab: F[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C]

  /** Collapse the structure by mapping each element to an element of a type that has a [[cats.Monoid]] */
  def bifoldMap[A, B, C](fab: F[A, B])(f: A => C, g: B => C)(implicit C: Monoid[C]): C =
    bifoldLeft(fab, C.empty)(
      (c: C, a: A) => C.combine(c, f(a)),
      (c: C, b: B) => C.combine(c, g(b))
    )

  def bifold[A: Monoid, B: Monoid](fab: F[A, B]): (A, B) =
    bifoldMap(fab)((_, Monoid[B].empty), (Monoid[A].empty, _))

  def compose[G[_, _]](implicit ev: Bifoldable[G]): Bifoldable[λ[(α, β) => F[G[α, β], G[α, β]]]] =
    new ComposedBifoldable[F, G] {
      val F = self
      val G = ev
    }
}

private[cats] trait ComposedBifoldable[F[_, _], G[_, _]] extends Bifoldable[λ[(α, β) => F[G[α, β], G[α, β]]]] {
  implicit def F: Bifoldable[F]
  implicit def G: Bifoldable[G]

  override def bifoldLeft[A, B, C](fab: F[G[A, B], G[A, B]], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    F.bifoldLeft(fab, c)(
      (c: C, gab: G[A, B]) => G.bifoldLeft(gab, c)(f, g),
      (c: C, gab: G[A, B]) => G.bifoldLeft(gab, c)(f, g)
    )

  override def bifoldRight[A, B, C](fab: F[G[A, B], G[A, B]], c: Eval[C])(f: (A, Eval[C]) => Eval[C],
                                                                          g: (B, Eval[C]) => Eval[C]): Eval[C] =
    F.bifoldRight(fab, c)(
      (gab: G[A, B], c: Eval[C]) => G.bifoldRight(gab, c)(f, g),
      (gab: G[A, B], c: Eval[C]) => G.bifoldRight(gab, c)(f, g)
    )
}
