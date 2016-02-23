package cats

/**
 * A type class abstracting over types that give rise to two independent [[cats.Foldable]]s.
 */
trait Bifoldable[F[_, _]] extends Any with Serializable { self =>
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

  def compose[G[_, _]](implicit ev: Bifoldable[G]): Bifoldable[Lambda[(A, B) => F[G[A, B], G[A, B]]]] =
    new CompositeBifoldable[F, G] {
      val F = self
      val G = ev
    }
}

object Bifoldable {
  def apply[F[_, _]](implicit F: Bifoldable[F]): Bifoldable[F] = F
}

trait CompositeBifoldable[F[_, _], G[_, _]] extends Bifoldable[Lambda[(A, B) => F[G[A, B], G[A, B]]]] {
  implicit def F: Bifoldable[F]
  implicit def G: Bifoldable[G]

  def bifoldLeft[A, B, C](fab: F[G[A, B], G[A, B]], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    F.bifoldLeft(fab, c)(
      (c: C, gab: G[A, B]) => G.bifoldLeft(gab, c)(f, g),
      (c: C, gab: G[A, B]) => G.bifoldLeft(gab, c)(f, g)
    )

  def bifoldRight[A, B, C](fab: F[G[A, B], G[A, B]], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
    F.bifoldRight(fab, c)(
      (gab: G[A, B], c: Eval[C]) => G.bifoldRight(gab, c)(f, g),
      (gab: G[A, B], c: Eval[C]) => G.bifoldRight(gab, c)(f, g)
    )
}
