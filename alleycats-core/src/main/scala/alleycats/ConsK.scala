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

package alleycats

import cats.SemigroupK
import simulacrum.typeclass

@typeclass trait ConsK[F[_]] extends Serializable {
  def cons[A](hd: A, tl: F[A]): F[A]
}

object ConsK {
  implicit def pureSemigroupKIsConsK[F[_]](implicit p: Pure[F], s: SemigroupK[F]): ConsK[F] =
    new ConsK[F] {
      def cons[A](hd: A, tl: F[A]): F[A] = s.combineK(p.pure(hd), tl)
    }

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[ConsK]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: ConsK[F]): ConsK[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllConsKOps[F[_], A](target: F[A])(implicit tc: ConsK[F]): AllOps[F, A] {
      type TypeClassType = ConsK[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = ConsK[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: ConsK[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A]
  trait ToConsKOps extends Serializable {
    implicit def toConsKOps[F[_], A](target: F[A])(implicit tc: ConsK[F]): Ops[F, A] {
      type TypeClassType = ConsK[F]
    } =
      new Ops[F, A] {
        type TypeClassType = ConsK[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToConsKOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
