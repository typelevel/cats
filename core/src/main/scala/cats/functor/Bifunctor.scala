package cats
package functor

/**
 * A type class of types which give rise to two independent, covariant
 * functors.
 */
trait Bifunctor[F[_, _]] extends Any with Serializable {

  /**
   * The quintessential method of the Bifunctor trait, it applies a
   * function to each "side" of the bifunctor.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val x: (List[String], Int) = (List("foo", "bar"), 3)
   * scala> x.bimap(_.headOption, _.toLong + 1)
   * res0: (Option[String], Long) = (Some(foo),4)
   * }}}
   */
  def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]

  /**
   * apply a function to the "left" functor
   */
  def leftMap[A, B, C](fab: F[A, B])(f: A => C): F[C, B]

  /**
   * apply a function ro the "right" functor
   */
  def rightMap[A, B, C](fab: F[A, B])(f: B => C): F[A, C]

  /** The composition of two Bifunctors is itself a Bifunctor */
  def compose[G[_, _]](implicit G0: Bifunctor[G]): Bifunctor[λ[(α, β) => F[G[α, β], G[α, β]]]]
}

object Bifunctor {
  def apply[F[_, _]](implicit ev: Bifunctor[F]): Bifunctor[F] = ev

  trait Default[F[_, _]] extends Bifunctor[F] { self =>

    def leftMap[A, B, C](fab: F[A, B])(f: A => C): F[C, B] = bimap(fab)(f, identity)

    def rightMap[A, B, C](fab: F[A, B])(f: B => C): F[A, C] = bimap(fab)(identity, f)

    def compose[G[_, _]](implicit G0: Bifunctor[G]): Bifunctor[λ[(α, β) => F[G[α, β], G[α, β]]]] =
      new ComposedBifunctor[F, G] {
        val F = self
        val G = G0
      }
  }
}

private[cats] trait ComposedBifunctor[F[_, _], G[_, _]]
    extends Bifunctor.Default[λ[(α, β) => F[G[α, β], G[α, β]]]] {
  def F: Bifunctor[F]
  def G: Bifunctor[G]

  override def bimap[A, B, C, D](fab: F[G[A, B], G[A, B]])(f: A => C, g: B => D): F[G[C, D], G[C, D]] = {
    val innerBimap: G[A, B] => G[C, D] = gab => G.bimap(gab)(f, g)
    F.bimap(fab)(innerBimap, innerBimap)
  }
}
