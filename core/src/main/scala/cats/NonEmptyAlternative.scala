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

@typeclass trait NonEmptyAlternative[F[_]] extends Applicative[F] with SemigroupK[F] { self =>

  /**
   * Lift `a` into `F[_]` and prepend it to `fa`.
   *
   * Example:
   * {{{
   * scala> NonEmptyAlternative[List].prependK(1, List(2, 3, 4))
   * res0: List[Int] = List(1, 2, 3, 4)
   * }}}
   */
  def prependK[A](a: A, fa: F[A]): F[A] = combineK(pure(a), fa)

  /**
   * Lift `a` into `F[_]` and append it to `fa`.
   *
   * Example:
   * {{{
   * scala> NonEmptyAlternative[List].appendK(List(1, 2, 3), 4)
   * res0: List[Int] = List(1, 2, 3, 4)
   * }}}
   */
  def appendK[A](fa: F[A], a: A): F[A] = combineK(fa, pure(a))

  override def compose[G[_]: Applicative]: NonEmptyAlternative[λ[α => F[G[α]]]] =
    new ComposedNonEmptyAlternative[F, G] {
      val F = self
      val G = Applicative[G]
    }
}

object NonEmptyAlternative {
  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[NonEmptyAlternative]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: NonEmptyAlternative[F]): NonEmptyAlternative[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllNonEmptyAlternativeOps[F[_], A](target: F[A])(implicit tc: NonEmptyAlternative[F]): AllOps[F, A] {
      type TypeClassType = NonEmptyAlternative[F]
    } = new AllOps[F, A] {
      type TypeClassType = NonEmptyAlternative[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: NonEmptyAlternative[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    // Note: `prependK` has to be added manually since simulacrum is not able to handle `self` as a second parameter.
    // def prependK(a: A): F[A] = typeClassInstance.prependK[A](a, self)
    def appendK(a: A): F[A] = typeClassInstance.appendK[A](self, a)
  }
  trait AllOps[F[_], A] extends Ops[F, A] with Applicative.AllOps[F, A] with SemigroupK.AllOps[F, A] {
    type TypeClassType <: NonEmptyAlternative[F]
  }
  trait ToNonEmptyAlternativeOps extends Serializable {
    implicit def toNonEmptyAlternativeOps[F[_], A](target: F[A])(implicit tc: NonEmptyAlternative[F]): Ops[F, A] {
      type TypeClassType = NonEmptyAlternative[F]
    } = new Ops[F, A] {
      type TypeClassType = NonEmptyAlternative[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToNonEmptyAlternativeOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */
}
