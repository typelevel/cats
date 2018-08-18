package cats

import simulacrum.typeclass

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

  def compose[G[_, _]](implicit ev: Bifoldable[G]): Bifoldable[λ[(α, β) => F[G[α, β], G[α, β]]]] =
    new ComposedBifoldable[F, G] {
      val F = self
      val G = ev
    }

  /**
   * Traverse this `F[A, B]` into a `G` structure for the effect only.
   *
   * [[Bifoldable.bitraverse_]] mirrors [[Foldable.traverse_]].
   *
   * Example:
   *
   * {{{
   * scala> import cats.data.State
   * scala> import cats.implicits._
   *
   * scala> type Acc[A] = State[Long, A]
   *
   * scala> def addInt(i: Int): Acc[Long] = State(acc => (acc + i.toLong, acc))
   *
   * scala> def addLong(l: Long): Acc[Long] = State(acc => (acc + l, acc))
   *
   * scala> val res = (3, 7L).bitraverse_(addInt, addLong)
   *
   * scala> res.runEmptyS.value
   * res0: Long = 10
   * }}}
   */
  def bitraverse_[G[_], A, B, C, D](fab: F[A, B])(f: A => G[C], g: B => G[D])(implicit G: Applicative[G]): G[Unit] =
    bifoldRight[A, B, G[Unit]](fab, Always(G.pure(())))(
      (a, lgu) => G.map2Eval(f(a), lgu)((_, _) => ()),
      (b, lgu) => G.map2Eval(g(b), lgu)((_, _) => ())
    ).value
}

private[cats] trait ComposedBifoldable[F[_, _], G[_, _]] extends Bifoldable[λ[(α, β) => F[G[α, β], G[α, β]]]] {
  implicit def F: Bifoldable[F]
  implicit def G: Bifoldable[G]

  override def bifoldLeft[A, B, C](fab: F[G[A, B], G[A, B]], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    F.bifoldLeft(fab, c)(
      (c: C, gab: G[A, B]) => G.bifoldLeft(gab, c)(f, g),
      (c: C, gab: G[A, B]) => G.bifoldLeft(gab, c)(f, g)
    )

  override def bifoldRight[A, B, C](fab: F[G[A, B], G[A, B]], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
    F.bifoldRight(fab, c)(
      (gab: G[A, B], c: Eval[C]) => G.bifoldRight(gab, c)(f, g),
      (gab: G[A, B], c: Eval[C]) => G.bifoldRight(gab, c)(f, g)
    )
}
