package cats
package arrow

import cats.functor.Strong

trait Arrow[F[_, _]] extends Split[F] with Strong[F] with Category[F] { self =>

  /**
   * Lift a function into the context of an Arrow
   */
  def lift[A, B](f: A => B): F[A, B]

  /**
   * Create a new arrow from an existing arrow that applies `f` to the input
   * of the original arrow and then applies `g` to the output.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.arrow.Arrow
   * scala> val fab: Double => Double = x => x + 0.3
   * scala> val f: Int => Double = x => x.toDouble / 2
   * scala> val g: Double => Double = x => x * 3
   * scala> val dimapArrow = Arrow[Function1].dimap(fab)(f)(g)
   * scala> dimapArrow(3)
   * res0: Double = 5.4
   * }}}
   */
  def dimap[A, B, C, D](fab: F[A, B])(f: C => A)(g: B => D): F[C, D] =
    compose(lift(g), andThen(lift(f), fab))

  /**
   * Create a new arrow that takes two inputs, but only modifies the first input
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.arrow.Arrow
   * scala> val f: Int => Int = _ * 2
   * scala> val fab = Arrow[Function1].first[Int,Int,Int](f)
   * scala> fab((2,3))
   * res0: (Int, Int) = (4,3)
   * }}}
   */
  def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)]

  /**
   * Create a new arrow that takes two inputs, but only modifies the second input
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.arrow.Arrow
   * scala> val f: Int => Int = _ * 2
   * scala> val fab = Arrow[Function1].second[Int,Int,Int](f)
   * scala> fab((2,3))
   * res0: (Int, Int) = (2,6)
   * }}}
   */
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
   * scala> import cats.implicits._
   * scala> import cats.arrow.Arrow
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
