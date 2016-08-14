package cats
package arrow

import cats.functor.Strong

import simulacrum.typeclass

@typeclass trait Arrow[F[_, _]] extends Split[F] with Strong[F] with Category[F] { self =>

  /**
   * Lift a function into the context of an Arrow
   */
  def lift[A, B](f: A => B): F[A, B]

  override def dimap[A, B, C, D](fab: F[A, B])(f: C => A)(g: B => D): F[C, D] =
    compose(lift(g), andThen(lift(f), fab))

  override def second[A, B, C](fa: F[A, B]): F[(C, A), (C, B)] = {
    def swap[X, Y]: F[(X, Y), (Y, X)] = lift[(X, Y), (Y, X)] { case (x, y) => (y, x) }
    compose(swap, compose(first[A, B, C](fa), swap))
  }

  override def split[A, B, C, D](f: F[A, B], g: F[C, D]): F[(A, C), (B, D)] =
    andThen(first(f), second(g))
}
