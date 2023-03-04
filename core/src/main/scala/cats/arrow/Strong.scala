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
 * Must obey the laws defined in cats.laws.StrongLaws.
 */
trait Strong[F[_, _]] extends Profunctor[F] {

  /**
   * Create a new `F` that takes two inputs, but only modifies the first input
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.arrow.Strong
   * scala> val f: Int => Int = _ * 2
   * scala> val fab = Strong[Function1].first[Int,Int,Int](f)
   * scala> fab((2,3))
   * res0: (Int, Int) = (4,3)
   * }}}
   */
  def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)]

  /**
   * Create a new `F` that takes two inputs, but only modifies the second input
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.arrow.Strong
   * scala> val f: Int => Int = _ * 2
   * scala> val fab = Strong[Function1].second[Int,Int,Int](f)
   * scala> fab((2,3))
   * res0: (Int, Int) = (2,6)
   * }}}
   */
  def second[A, B, C](fa: F[A, B]): F[(C, A), (C, B)]
}

object Strong {

  /**
   * Summon an instance of [[Strong]] for `F`.
   */
  @inline def apply[F[_, _]](implicit instance: Strong[F]): Strong[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllStrongOps[F[_, _], A, B](target: F[A, B])(implicit tc: Strong[F]): AllOps[F, A, B] {
      type TypeClassType = Strong[F]
    } =
      new AllOps[F, A, B] {
        type TypeClassType = Strong[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_, _], A, B] extends Serializable {
    type TypeClassType <: Strong[F]
    def self: F[A, B]
    val typeClassInstance: TypeClassType
    def first[C]: F[(A, C), (B, C)] = typeClassInstance.first[A, B, C](self)
    def second[C]: F[(C, A), (C, B)] = typeClassInstance.second[A, B, C](self)
  }
  trait AllOps[F[_, _], A, B] extends Ops[F, A, B] with Profunctor.AllOps[F, A, B] {
    type TypeClassType <: Strong[F]
  }
  trait ToStrongOps extends Serializable {
    implicit def toStrongOps[F[_, _], A, B](target: F[A, B])(implicit tc: Strong[F]): Ops[F, A, B] {
      type TypeClassType = Strong[F]
    } =
      new Ops[F, A, B] {
        type TypeClassType = Strong[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToStrongOps

}
