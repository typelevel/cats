package cats
package arrow

import cats.functor.Strong

trait Arrow[F[_, _]] extends Split[F] with Strong[F] with Category[F] { self =>

  def lift[A, B](f: A => B): F[A, B]

  override def lmap[A, B, C](fab: F[A, B])(f: C => A): F[C, B] =
    andThen(lift(f), fab)

  override def rmap[A, B, C](fab: F[A, B])(f: B => C): F[A, C] =
    compose(lift(f), fab)

  def second[A, B, C](fa: F[A, B]): F[(C, A), (C, B)] = {
    def swap[X, Y] = lift[(X, Y), (Y, X)] { case (x, y) => (y, x) }
    compose(swap, (compose(first[A, B, C](fa), swap)))
  }
}

object Arrow {
  def apply[F[_, _]](implicit ev: Arrow[F]): Arrow[F] = ev
}
