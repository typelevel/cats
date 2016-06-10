package cats
package arrow

import cats.data.Xor

trait Choice[F[_, _]] extends Category[F] {

  /**
   * Given two `F`s (`f` and `g`) with a common target type, create a new `F`
   * with the same target type, but with a source type of either `f`'s source
   * type OR `g`'s source type.
   *
   * Example:
   * {{{
   * scala> import cats.data.Xor
   * scala> import cats.implicits._
   * scala> val b: Boolean => String = _ + " is a boolean"
   * scala> val i: Int => String =  _ + " is an integer"
   * scala> val f: (Boolean Xor Int) => String = Choice[Function1].choice(b, i)
   *
   * scala> f(Xor.right(3))
   * res0: String = 3 is an integer
   *
   * scala> f(Xor.left(false))
   * res0: String = false is a boolean
   * }}}
   */
  def choice[A, B, C](f: F[A, C], g: F[B, C]): F[Xor[A, B], C]

  /**
   * An `F` that, given a source `A` on either the right or left side, will
   * return that same `A` object.
   *
   * Example:
   * {{{
   * scala> import cats.data.Xor
   * scala> import cats.implicits._
   * scala> val f: (Int Xor Int) => Int = Choice[Function1].codiagonal[Int]
   *
   * scala> f(Xor.right(3))
   * res0: Int = 3
   *
   * scala> f(Xor.left(3))
   * res1: Int = 3
   * }}}
   */
  def codiagonal[A]: F[Xor[A, A], A] = choice(id, id)
}

object Choice {
  def apply[F[_, _]](implicit F: Choice[F]): Choice[F] = F
}
