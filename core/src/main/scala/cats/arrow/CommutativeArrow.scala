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

import simulacrum.typeclass

/**
 * In a Commutative Arrow F[_, _], the split operation (or `***`) is commutative,
 * which means that there is non-interference between the effect of the paired arrows.
 *
 * Must obey the laws in CommutativeArrowLaws
 */
@typeclass trait CommutativeArrow[F[_, _]] extends Arrow[F]

object CommutativeArrow {

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[CommutativeArrow]] for `F`.
   */
  @inline def apply[F[_, _]](implicit instance: CommutativeArrow[F]): CommutativeArrow[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllCommutativeArrowOps[F[_, _], A, B](
      target: F[A, B]
    )(implicit tc: CommutativeArrow[F]): AllOps[F, A, B] {
      type TypeClassType = CommutativeArrow[F]
    } =
      new AllOps[F, A, B] {
        type TypeClassType = CommutativeArrow[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_, _], A, B] extends Serializable {
    type TypeClassType <: CommutativeArrow[F]
    def self: F[A, B]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_, _], A, B] extends Ops[F, A, B] with Arrow.AllOps[F, A, B] {
    type TypeClassType <: CommutativeArrow[F]
  }
  trait ToCommutativeArrowOps extends Serializable {
    implicit def toCommutativeArrowOps[F[_, _], A, B](target: F[A, B])(implicit tc: CommutativeArrow[F]): Ops[F, A, B] {
      type TypeClassType = CommutativeArrow[F]
    } =
      new Ops[F, A, B] {
        type TypeClassType = CommutativeArrow[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToCommutativeArrowOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
