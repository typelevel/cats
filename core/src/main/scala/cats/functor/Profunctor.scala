package cats
package functor

/**
 * A [[Profunctor]] is a [[Contravariant]] functor on its first type parameter
 * and a [[Functor]] on its second type parameter.
 */
trait Profunctor[F[_, _]] { self =>
  /**
   * contramap on the first type parameter
   */
  def lmap[A, B, C](fab: F[A, B])(f: C => A): F[C, B]

  /**
   * map on the second type parameter
   */
  def rmap[A, B, C](fab: F[A, B])(f: B => C): F[A, C]

  /**
   * contramap on the first type parameter and map on the second type parameter
   */
  def dimap[A, B, C, D](fab: F[A, B])(f: C => A)(g: B => D): F[C, D] = rmap(lmap(fab)(f))(g)
}


object Profunctor {
  def apply[F[_, _]](implicit ev: Profunctor[F]): Profunctor[F] = ev
}
