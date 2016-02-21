package cats
package arrow

import cats.functor.Strong

trait Arrow[F[_, _]] extends Split[F] with Strong[F] with Category[F] {
  self =>

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
    * scala> import cats.std.function._
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
    * scala> import cats.std.function._
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
    * scala> import cats.std.function._
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
   * scala> import cats.std.function._
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

  // helper arrow: duplicates value
  def dupArrow[A]: F[A, (A, A)] = lift[A, (A, A)] { a => (a, a) }

  // helper arrow: applies function value to value
  def applyArrow[A, B]: F[(A, A => B), B] =
    lift {
      case (a, a2b) =>
        a2b(a)
    }

  // uncurried version of flatDef
  def flatDefine[A, B, C](afb: F[A, B]): F[(A, B), C] => F[A, C] =
    abfc =>
      andThen(andThen(dupArrow, second[A, B, A](afb)), abfc)

  // curried version of flatDefine
  def flatDef[A, B, C](afb: F[A, B]): F[B, A => C] => F[A, C] =
    bfa2c =>
      andThen(andThen(dupArrow, second[A, A => C, A](andThen(afb, bfa2c))), applyArrow[A, C])

  // uncurried version of `def`
  def define[A, B, C](a2b: A => B): F[(A, B), C] => F[A, C] =
    flatDefine(lift(a2b))

  // curried version of define
  def `def`[A, B, C](a2b: A => B): F[B, A => C] => F[A, C] =
    flatDef(lift(a2b))

  // uncurried version of `val`
  def value[A, B, C](b: B): F[(A, B), C] => F[A, C] =
    define { _ => b }

  // curried version of value
  def `val`[A, B, C](b: B): F[B, A => C] => F[A, C] =
    `def` { _ => b }
}

object Arrow {
  def apply[F[_, _]](implicit ev: Arrow[F]): Arrow[F] = ev
}
