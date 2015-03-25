package cats
package functor

/**
 * A typeclass of types which give rise to two independent, covariant
 * functors.
 */
trait Bifunctor[F[_, _]] extends Serializable { self =>

  /**
   * The quintessential method of the Bifunctor trait, it applies a
   * function to each "side" of the bifunctor.
   */
  def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]

  // derived methods
  /**
   * apply a function to the "left" functor
   */
  def leftMap[A,B,C](fab: F[A, B])(f: A => C): F[C,B] = bimap(fab)(f, identity)

  /**
   * apply a function ro the "right" functor
   */
  def rightMap[A,B,C](fab: F[A, B])(f: B => C): F[A,C] = bimap(fab)(identity, f)
}

object Bifunctor {
  def apply[F[_, _]](implicit ev: Bifunctor[F]): Bifunctor[F] = ev
}
