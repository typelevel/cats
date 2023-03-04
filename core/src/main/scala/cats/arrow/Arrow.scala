/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats
package arrow

/**
 * Must obey the laws defined in cats.laws.ArrowLaws.
 */
trait Arrow[F[_, _]] extends Category[F] with Strong[F] { self =>

  /**
   *  Lift a function into the context of an Arrow.
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
   * scala> import cats.syntax.all._
   * scala> import cats.arrow.Arrow
   * scala> val toLong: Int => Long = _.toLong
   * scala> val toDouble: Float => Double = _.toDouble
   * scala> val f: ((Int, Float)) => (Long, Double) = Arrow[Function1].split(toLong, toDouble)
   * scala> f((3, 4.0f))
   * res0: (Long, Double) = (3,4.0)
   * }}}
   *
   * Note that the arrow laws do not guarantee the non-interference between the _effects_ of
   * `f` and `g` in the context of F. This means that `f *** g` may not be equivalent to `g *** f`.
   */

  def split[A, B, C, D](f: F[A, B], g: F[C, D]): F[(A, C), (B, D)] =
    andThen(first(f), second(g))

  /**
   * Create a new computation `F` that merge outputs of `f` and `g` both having the same input
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
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

  def merge[A, B, C](f: F[A, B], g: F[A, C]): F[A, (B, C)] =
    andThen(lift((x: A) => (x, x)), split(f, g))
}

object Arrow {

  /**
   * Summon an instance of [[Arrow]] for `F`.
   */
  @inline def apply[F[_, _]](implicit instance: Arrow[F]): Arrow[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllArrowOps[F[_, _], A, B](target: F[A, B])(implicit tc: Arrow[F]): AllOps[F, A, B] {
      type TypeClassType = Arrow[F]
    } =
      new AllOps[F, A, B] {
        type TypeClassType = Arrow[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_, _], A, B] extends Serializable {
    type TypeClassType <: Arrow[F]
    def self: F[A, B]
    val typeClassInstance: TypeClassType
    def split[C, D](g: F[C, D]): F[(A, C), (B, D)] = typeClassInstance.split[A, B, C, D](self, g)
    def ***[C, D](g: F[C, D]): F[(A, C), (B, D)] = typeClassInstance.split[A, B, C, D](self, g)
    def merge[C](g: F[A, C]): F[A, (B, C)] = typeClassInstance.merge[A, B, C](self, g)
    def &&&[C](g: F[A, C]): F[A, (B, C)] = typeClassInstance.merge[A, B, C](self, g)
  }
  trait AllOps[F[_, _], A, B] extends Ops[F, A, B] with Category.AllOps[F, A, B] with Strong.AllOps[F, A, B] {
    type TypeClassType <: Arrow[F]
  }
  trait ToArrowOps extends Serializable {
    implicit def toArrowOps[F[_, _], A, B](target: F[A, B])(implicit tc: Arrow[F]): Ops[F, A, B] {
      type TypeClassType = Arrow[F]
    } =
      new Ops[F, A, B] {
        type TypeClassType = Arrow[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToArrowOps

}
