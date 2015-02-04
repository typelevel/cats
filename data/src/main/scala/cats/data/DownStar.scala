package cats.data

import cats.functor.Profunctor
import cats.{Functor, Monad}

final case class DownStar[F[_], A, B](fab: F[A] => B){
  def dimap[C, D](f: C => A)(g: B => D)(implicit F: Functor[F]): DownStar[F, C, D] =
    DownStar(fc => g(fab(F.map(fc)(f))))

  def lmap[C](f: C => A)(implicit F: Functor[F]): DownStar[F, C, B] =
    DownStar(fc => fab(F.map(fc)(f)))

  def rmap[C](f: B => C): DownStar[F, A, C] =
    DownStar(f compose fab)

  def flatMap[C](f: B => DownStar[F, A, C]): DownStar[F, A, C] =
    DownStar(fa => f(fab(fa)).fab(fa))
}

object DownStar extends DownStarInstances {
  def pure[F[_], A, B](x: B): DownStar[F, A, B] =
    DownStar(_ => x)
}

sealed abstract class DownStarInstances {
  implicit def downStarProfunctor[F[_]: Functor] = new Profunctor[DownStar[F, ?, ?]] {
    override def lmap[A, B, C](fab: DownStar[F, A, B])(f: C => A): DownStar[F, C, B] =
      fab.lmap(f)

    override def rmap[A, B, C](fab: DownStar[F, A, B])(f: B => C): DownStar[F, A, C] =
      fab.rmap(f)

    override def dimap[A, B, C, D](fab: DownStar[F, A, B])(f: C => A)(g: B => D): DownStar[F, C, D] =
      fab.dimap(f)(g)
  }

  implicit def downStarMonad[F[_], A]: Monad[DownStar[F, A, ?]] = new Monad[DownStar[F, A, ?]] {
    def pure[B](x: B): DownStar[F, A, B] =
      DownStar.pure(x)

    def flatMap[B, C](fa: DownStar[F, A, B])(f: B => DownStar[F, A, C]): DownStar[F, A, C] =
      fa.flatMap(f)
  }
}