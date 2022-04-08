/*
 * Copyright (c) 2022 Typelevel
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
import simulacrum.typeclass

@typeclass trait Distributive[F[_]] extends Functor[F] { self =>

  /**
   * Given a function which returns a distributive `F`, apply that value across the structure G.
   */
  def distribute[G[_]: Functor, A, B](ga: G[A])(f: A => F[B]): F[G[B]]

  /**
   * Given a Functor G which wraps some distributive F, distribute F across the G.
   */
  def cosequence[G[_]: Functor, A](ga: G[F[A]]): F[G[A]] = distribute(ga)(identity)

  // Distributive composes
  def compose[G[_]](implicit G0: Distributive[G]): Distributive[λ[α => F[G[α]]]] =
    new ComposedDistributive[F, G] {
      implicit def F = self
      implicit def G = G0
    }
}

object Distributive {

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[Distributive]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Distributive[F]): Distributive[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllDistributiveOps[F[_], A](target: F[A])(implicit tc: Distributive[F]): AllOps[F, A] {
      type TypeClassType = Distributive[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = Distributive[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: Distributive[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A] with Functor.AllOps[F, A] {
    type TypeClassType <: Distributive[F]
  }
  trait ToDistributiveOps extends Serializable {
    implicit def toDistributiveOps[F[_], A](target: F[A])(implicit tc: Distributive[F]): Ops[F, A] {
      type TypeClassType = Distributive[F]
    } =
      new Ops[F, A] {
        type TypeClassType = Distributive[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToDistributiveOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
