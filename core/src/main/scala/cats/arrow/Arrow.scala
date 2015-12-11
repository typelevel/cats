package cats
package arrow

import cats.functor.Strong

trait Arrow[F[_, _]] extends Split[F] with Strong[F] with Category[F] { self =>

  def lift[A, B](f: A => B): F[A, B]

  def dimap[A, B, C, D](fab: F[A, B])(f: C => A)(g: B => D): F[C, D] =
    compose(lift(g), andThen(lift(f), fab))

  def second[A, B, C](fa: F[A, B]): F[(C, A), (C, B)] = {
    def swap[X, Y]: F[(X, Y), (Y, X)] = lift[(X, Y), (Y, X)] { case (x, y) => (y, x) }
    compose(swap, compose(first[A, B, C](fa), swap))
  }

  /**
   * Create a new arrow that splits its input between the `f` and `g` arrows
   * and combines the output of each.
   *
   * Example:
   * {{{
   * scala> import cats.std.function._
   * scala> val toLong: Int => Long = _.toLong
   * scala> val toDouble: Float => Double = _.toDouble
   * scala> val f: ((Int, Float)) => (Long, Double) = Arrow[Function1].split(toLong, toDouble)
   * scala> f((3, 4.0f))
   * res0: (Long, Double) = (3,4.0)
   * }}}
   */
  def split[A, B, C, D](f: F[A, B], g: F[C, D]): F[(A, C), (B, D)] =
    andThen(first(f), second(g))
}

object Arrow {
  def apply[F[_, _]](implicit ev: Arrow[F]): Arrow[F] = ev
}
