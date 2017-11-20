package cats

import simulacrum.typeclass

@typeclass trait Alternative[F[_]] extends Applicative[F] with MonoidK[F] { self =>
  /**
    * Fold over the inner structure to combine all of the values with
    * our combine method inherited from MonoidK. The result is for us
    * to accumulate all of the "interesting" values of the inner G, so
    * if G is Option, we collect all the Some values, if G is Either,
    * we collect all the Right values, etc.
    */
  def unite[G[_], A](fga: F[G[A]])(implicit FM: Monad[F], G: Foldable[G]): F[A] =
    FM.flatMap(fga) { ga =>
      G.foldLeft(ga, empty[A])((acc, a) => combineK(acc, pure(a)))
    }

  /** Separate the inner foldable values into the "lefts" and "rights" */
  def separate[G[_, _], A, B](fgab: F[G[A, B]])(implicit FM: Monad[F], G: Bifoldable[G]): (F[A], F[B]) = {
    val as = FM.flatMap(fgab)(gab => G.bifoldMap(gab)(pure, _ => empty[A])(algebra[A]))
    val bs = FM.flatMap(fgab)(gab => G.bifoldMap(gab)(_ => empty[B], pure)(algebra[B]))
    (as, bs)
  }

  /** Return ().pure[F] if `condition` is true, `empty` otherwise */
  def guard(condition: Boolean): F[Unit] =
    if (condition) pure(()) else empty

  override def compose[G[_]: Applicative]: Alternative[λ[α => F[G[α]]]] =
    new ComposedAlternative[F, G] {
      val F = self
      val G = Applicative[G]
    }
}
