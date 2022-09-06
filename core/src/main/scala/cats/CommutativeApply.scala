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

import cats.kernel.CommutativeSemigroup

/**
 * Commutative Apply.
 *
 * Further than an Apply, which just allows composition of independent effectful functions,
 * in a Commutative Apply those functions can be composed in any order, which guarantees
 * that their effects do not interfere.
 *
 * Must obey the laws defined in cats.laws.CommutativeApplyLaws.
 */
trait CommutativeApply[F[_]] extends Apply[F]

object CommutativeApply {
  def commutativeSemigroupFor[F[_]: CommutativeApply, A: CommutativeSemigroup]: CommutativeSemigroup[F[A]] =
    CommutativeApply[F].map2(_, _)(CommutativeSemigroup[A].combine)

  /**
   * Summon an instance of [[CommutativeApply]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: CommutativeApply[F]): CommutativeApply[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllCommutativeApplyOps[F[_], A](target: F[A])(implicit tc: CommutativeApply[F]): AllOps[F, A] {
      type TypeClassType = CommutativeApply[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = CommutativeApply[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: CommutativeApply[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A] with Apply.AllOps[F, A] {
    type TypeClassType <: CommutativeApply[F]
  }
  trait ToCommutativeApplyOps extends Serializable {
    implicit def toCommutativeApplyOps[F[_], A](target: F[A])(implicit tc: CommutativeApply[F]): Ops[F, A] {
      type TypeClassType = CommutativeApply[F]
    } =
      new Ops[F, A] {
        type TypeClassType = CommutativeApply[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToCommutativeApplyOps

}
