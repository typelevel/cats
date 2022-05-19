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

import simulacrum.typeclass

/**
 * Commutative FlatMap.
 *
 * Further than a FlatMap, which just allows composition of dependent effectful functions,
 * in a Commutative FlatMap those functions can be composed in any order, which guarantees
 * that their effects do not interfere.
 *
 * Must obey the laws defined in cats.laws.CommutativeFlatMapLaws.
 */
@typeclass trait CommutativeFlatMap[F[_]] extends FlatMap[F] with CommutativeApply[F]

object CommutativeFlatMap {

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[CommutativeFlatMap]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: CommutativeFlatMap[F]): CommutativeFlatMap[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllCommutativeFlatMapOps[F[_], A](target: F[A])(implicit tc: CommutativeFlatMap[F]): AllOps[F, A] {
      type TypeClassType = CommutativeFlatMap[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = CommutativeFlatMap[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: CommutativeFlatMap[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A] with FlatMap.AllOps[F, A] with CommutativeApply.AllOps[F, A] {
    type TypeClassType <: CommutativeFlatMap[F]
  }
  trait ToCommutativeFlatMapOps extends Serializable {
    implicit def toCommutativeFlatMapOps[F[_], A](target: F[A])(implicit tc: CommutativeFlatMap[F]): Ops[F, A] {
      type TypeClassType = CommutativeFlatMap[F]
    } =
      new Ops[F, A] {
        type TypeClassType = CommutativeFlatMap[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToCommutativeFlatMapOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
