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

  /**
    * Create a new computation `F` that merge outputs of `f` and `g` both having the same input
    *
    * Example:
    * {{{
    * scala> import cats.implicits._
    * scala> val addEmpty: Int => Int = _ + 0
    * scala> val multiplyEmpty: Int => Double= _ * 1d
    * scala> val f: Int => (Int, Double) = addEmpty &&& multiplyEmpty
    * scala> f(1)
    * res0: (Int, Double) = (1,1.0)
    * }}}
    *
    * Note that the arrow laws do not guarantee the non-interference between the _effects_ of
    *  `f` and `g` in the context of F. This means that `f &&& g` may not be equivalent to `g &&& f`.
    */
  @simulacrum.op("&&&", alias = true)
  def merge[A, B, C](f: F[A, B], g: F[A, C]): F[A, (B, C)] = {
    andThen(lift((x: A) => (x, x)), split(f, g))
  }

  /**
    * Create a new computation `F` that apply f andThen biforks the result.
    * On one way it is applied to g and on the other it is passed through.
    * The final result is a tuple
    * <br/><pre>
    *                                 /------- out1 -----\
    *  input -->  out1= f(input) --> {                    }--->(out1,out2)
    *                                 \--out2 = g(out1)--/
    *             </pre>
    * Example:
    * {{{
    * scala> import cats.implicits._
    * scala> val twoTimes: Int => Double = _ * 2d
    * scala> val fiveTimes: Double => Double= _ * 5
    * scala> val f: Int => (Double, Double) = twoTimes -< fiveTimes
    * scala> f(2)
    * res0: (Double, Double) = (4.0,20.0)
    * }}}
    *
    */
  @simulacrum.op("-<", alias = true)
  def combineAndBypass[A, B, C](f: F[A, B], g: F[B, C]): F[A, (B, C)] = {
    andThen(andThen( f, lift((x: B) => (x, x)) ), split(id, g))
  }
}
