package cats
package functor

/**
 * A type class of types which give rise to two independent, covariant
 * functors.
 */
trait Bifunctor[F[_, _]] extends Any with Serializable { self =>

  /**
   * The quintessential method of the Bifunctor trait, it applies a
   * function to each "side" of the bifunctor.
   */
  def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]

  // derived methods
  /**
   * apply a function to the "left" functor
   */
  def leftMap[A, B, C](fab: F[A, B])(f: A => C): F[C, B] = bimap(fab)(f, identity)

  /**
   * apply a function ro the "right" functor
   */
  def rightMap[A, B, C](fab: F[A, B])(f: B => C): F[A, C] = bimap(fab)(identity, f)

  /** The composition of two Bifunctors is itself a Bifunctor */
  def compose[G[_, _]](implicit G0: Bifunctor[G]): Bifunctor[λ[(α, β) => F[G[α, β], G[α, β]]]] =
    new ComposedBifunctor[F, G] {
      val F = self
      val G = G0
    }
}

object Bifunctor {
  def apply[F[_, _]](implicit ev: Bifunctor[F]): Bifunctor[F] = ev
}

private[cats] trait ComposedBifunctor[F[_, _], G[_, _]]
    extends Bifunctor[λ[(A, B) => F[G[A, B], G[A, B]]]] {
  def F: Bifunctor[F]
  def G: Bifunctor[G]

  override def bimap[A, B, C, D](fab: F[G[A, B], G[A, B]])(f: A => C, g: B => D): F[G[C, D], G[C, D]] = {
    val innerBimap: G[A, B] => G[C, D] = gab => G.bimap(gab)(f, g)
    F.bimap(fab)(innerBimap, innerBimap)
  }
}
