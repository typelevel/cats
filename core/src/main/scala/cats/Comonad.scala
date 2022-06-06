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

/**
 * Comonad
 *
 * Comonad is the dual of Monad. Whereas Monads allow for the composition of effectful functions,
 * Comonads allow for composition of functions that extract the value from their context.
 *
 * Must obey the laws defined in cats.laws.ComonadLaws.
 */
trait Comonad[F[_]] extends CoflatMap[F] {

  /**
   * `extract` is the dual of `pure` on Monad (via `Applicative`)
   * and extracts the value from its context
   *
   * Example:
   * {{{
   * scala> import cats.Id
   * scala> import cats.Comonad
   * scala> val id: Id[Int] = 3
   * scala> Comonad[Id].extract(id)
   * res0: cats.Id[Int] = 3
   * }}}
   */
  def extract[A](x: F[A]): A
}

object Comonad {

  /**
   * Summon an instance of [[Comonad]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Comonad[F]): Comonad[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllComonadOps[F[_], A](target: F[A])(implicit tc: Comonad[F]): AllOps[F, A] {
      type TypeClassType = Comonad[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = Comonad[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: Comonad[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def extract: A = typeClassInstance.extract[A](self)
  }
  trait AllOps[F[_], A] extends Ops[F, A] with CoflatMap.AllOps[F, A] {
    type TypeClassType <: Comonad[F]
  }
  trait ToComonadOps extends Serializable {
    implicit def toComonadOps[F[_], A](target: F[A])(implicit tc: Comonad[F]): Ops[F, A] {
      type TypeClassType = Comonad[F]
    } =
      new Ops[F, A] {
        type TypeClassType = Comonad[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToComonadOps

}
