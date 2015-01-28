package cats
package functor

trait Profunctor[F[_, _]] { self =>
  def lmap[A, B, C](fab: F[A, B])(f: C => A): F[C, B]
  def rmap[A, B, C](fab: F[A, B])(f: B => C): F[A, C]
  def dimap[A, B, C, D](fab: F[A, B])(f: C => A)(g: B => D): F[C, D] = rmap(lmap(fab)(f))(g)
}


object Profunctor {

  def apply[F[_, _]](implicit ev: Profunctor[F]): Profunctor[F] = ev

  case class UpStar[F[_]: Functor, A, B](f: A => F[B])

  case class DownStar[F[_]: Functor, A, B](f: F[A] => B)

  def upStar[F[_]: Functor]: Profunctor[UpStar[F, ?, ?]] =
    new Profunctor[UpStar[F, ?, ?]] {
      def lmap[A, B, C](fab: UpStar[F, A, B])(f: C => A): UpStar[F, C, B] =
        UpStar(fab.f compose f)
      def rmap[A, B, C](fab: UpStar[F, A, B])(f: B => C): UpStar[F, A, C] =
        UpStar(a => Functor[F].map(fab.f(a))(f))
    }

  def downStar[F[_]: Functor]: Profunctor[DownStar[F, ?, ?]] =
    new Profunctor[DownStar[F, ?, ?]] {
      def lmap[A, B, C](fab: DownStar[F, A, B])(f: C => A): DownStar[F, C, B] =
        DownStar(fc => fab.f(Functor[F].map(fc)(f)))
      def rmap[A, B, C](fab: DownStar[F, A, B])(f: B => C): DownStar[F, A, C] =
        DownStar(f compose fab.f)
    }
}
