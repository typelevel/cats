package cats
package arrow

import simulacrum.typeclass

/**
 * Must obey the laws defined in cats.laws.ArrowLaws.
 */
@typeclass trait Arrow[F[_, _]] extends Category[F] with Strong[F] { self =>

  /**
   * Lift a function into the context of an Arrow.
   *
   * In the reference articles "Arrows are Promiscuous...", and in the corresponding Haskell
   * library `Control.Arrow`, this function is called `arr`.
   */
  def lift[A, B](f: A => B): F[A, B]

  override def id[A]: F[A, A] = lift(identity)

  override def dimap[A, B, C, D](fab: F[A, B])(f: C => A)(g: B => D): F[C, D] =
    compose(lift(g), andThen(lift(f), fab))

  override def second[A, B, C](fa: F[A, B]): F[(C, A), (C, B)] = {
    def swap[X, Y]: F[(X, Y), (Y, X)] = lift[(X, Y), (Y, X)] { case (x, y) => (y, x) }
    compose(swap, compose(first[A, B, C](fa), swap))
  }

  /**
   * Create a new computation `F` that splits its input between `f` and `g`
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
   *
   * Note that the arrow laws do not guarantee the non-interference between the _effects_ of
   *  `f` and `g` in the context of F. This means that `f *** g` may not be equivalent to `g *** f`.
   */
  @simulacrum.op("***", alias = true)
  def split[A, B, C, D](f: F[A, B], g: F[C, D]): F[(A, C), (B, D)] =
    andThen(first(f), second(g))
}
object Arrow {
  /**
   * Creates a semigroupal functor for `F`, holding domain fixed and combining
   * over the codomain.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.arrow.Arrow.catsSemigroupalForArrow
   * scala> val toLong: Int => Long = _.toLong
   * scala> val double: Int => Int = 2*_
   * scala> val f: Int => (Long, Int) = catsSemigroupalForArrow.product(toLong, double)
   * scala> f(3)
   * res0: (Long, Int) = (3,6)
   * }}}
   */
  implicit def catsSemigroupalForArrow[F[_, _], A](implicit F: Arrow[F]): Semigroupal[F[A, ?]] = new Apply[F[A, ?]] {
    def map[B, C](fb: F[A, B])(f: B => C): F[A, C] = F.rmap(fb)(f)
    def ap[B, C](ff: F[A, B => C])(fb: F[A, B]): F[A, C] =
      F.rmap(F.andThen(F.lift((x: A) => (x, x)), F.split(ff, fb)))(tup => tup._1(tup._2))
  }
}
