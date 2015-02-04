package cats
package functor

/**
 * A [[Profunctor]] is a [[Contravariant]] functor on its first type parameter
 * and a [[Functor]] on its second type parameter.
 *
 * Minimum implementation requires a defining dimap or lmap and rmap
 */
trait Profunctor[F[_, _]] { self =>
  /**
   * contramap on the first type parameter
   */
  def lmap[A, B, C](fab: F[A, B])(f: C => A): F[C, B] =
    dimap(fab)(f)(identity)

  /**
   * map on the second type parameter
   */
  def rmap[A, B, C](fab: F[A, B])(f: B => C): F[A, C] =
    dimap[A, B, A, C](fab)(identity)(f)

  /**
   * contramap on the first type parameter and map on the second type parameter
   */
  def dimap[A, B, C, D](fab: F[A, B])(f: C => A)(g: B => D): F[C, D] =
    rmap(lmap(fab)(f))(g)
}


object Profunctor {

  def apply[F[_, _]](implicit ev: Profunctor[F]): Profunctor[F] = ev

  case class UpStar[F[_]: Functor, A, B](f: A => F[B])

  case class DownStar[F[_]: Functor, A, B](f: F[A] => B)

  def upStar[F[_]: Functor]: Profunctor[UpStar[F, ?, ?]] =
    new Profunctor[UpStar[F, ?, ?]] {
      override def lmap[A, B, C](fab: UpStar[F, A, B])(f: C => A): UpStar[F, C, B] =
        UpStar(fab.f compose f)
      override def rmap[A, B, C](fab: UpStar[F, A, B])(f: B => C): UpStar[F, A, C] =
        UpStar(a => Functor[F].map(fab.f(a))(f))
    }

  def downStar[F[_]: Functor]: Profunctor[DownStar[F, ?, ?]] =
    new Profunctor[DownStar[F, ?, ?]] {
      override def lmap[A, B, C](fab: DownStar[F, A, B])(f: C => A): DownStar[F, C, B] =
        DownStar(fc => fab.f(Functor[F].map(fc)(f)))
      override def rmap[A, B, C](fab: DownStar[F, A, B])(f: B => C): DownStar[F, A, C] =
        DownStar(f compose fab.f)
    }
}
