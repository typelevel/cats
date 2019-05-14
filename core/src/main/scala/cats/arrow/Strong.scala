package cats
package arrow

import simulacrum.typeclass

/**
 * Must obey the laws defined in cats.laws.StrongLaws.
 */
@typeclass trait Strong[|==>[_, _]] extends Profunctor[|==>] {

  /**
   * Create a new `F` that takes two inputs, but only modifies the first input
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.arrow.Strong
   * scala> val f: Int => Int = _ * 2
   * scala> val fab = Strong[Function1].first[Int,Int,Int](f)
   * scala> fab((2,3))
   * res0: (Int, Int) = (4,3)
   * }}}
   */
  def first[A, B, C](fa: A |==> B): (A, C) |==> (B, C)

  /**
   * Create a new `F` that takes two inputs, but only modifies the second input
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.arrow.Strong
   * scala> val f: Int => Int = _ * 2
   * scala> val fab = Strong[Function1].second[Int,Int,Int](f)
   * scala> fab((2,3))
   * res0: (Int, Int) = (2,6)
   * }}}
   */
  def second[A, B, C](fa: A |==> B): (C, A) |==> (C, B)
}
