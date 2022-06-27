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
 * [[Decidable]] functors are functors that supply
 * a `decide` operation allowing choices to be made.
 *
 * This is comparable to [[Alternative]] in the
 * covariant case.
 *
 * Must obey laws in cats.laws.DecidableLaws.
 *
 * Based on ekmett's contravariant library:
 * https://hackage.haskell.org/package/contravariant-1.4/docs/Data-Functor-Contravariant-Divisible.html#g:2
 */
trait Decidable[F[_]] extends ContravariantMonoidal[F] {
  def sum[A, B](fa: F[A], fb: F[B]): F[Either[A, B]]

  def decide[A, B, C](fa: F[A], fb: F[B])(f: C => Either[A, B]): F[C] =
    contramap(sum(fa, fb))(f)

  def chosen[B, C](fb: F[B], fc: F[C]): F[Either[B, C]] = sum(fb, fc)
  def lose[A](f: A => Nothing): F[A] = contramap[Nothing, A](zero)(f)
  def zero: F[Nothing]
}

object Decidable {

  /**
   * Summon an instance of [[Decidable]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Decidable[F]): Decidable[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllDecidableOps[F[_], A](target: F[A])(implicit tc: Decidable[F]): AllOps[F, A] {
      type TypeClassType = Decidable[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = Decidable[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: Decidable[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def sum[B](fb: F[B]): F[Either[A, B]] = typeClassInstance.sum[A, B](self, fb)
    def decide[B, C](fb: F[B])(f: C => Either[A, B]): F[C] = typeClassInstance.decide[A, B, C](self, fb)(f)
    def chosen[B](fc: F[B]): F[Either[A, B]] = typeClassInstance.chosen[A, B](self, fc)
  }
  trait AllOps[F[_], A] extends Ops[F, A] with ContravariantMonoidal.AllOps[F, A] {
    type TypeClassType <: Decidable[F]
  }
  trait ToDecidableOps extends Serializable {
    implicit def toDecidableOps[F[_], A](target: F[A])(implicit tc: Decidable[F]): Ops[F, A] {
      type TypeClassType = Decidable[F]
    } =
      new Ops[F, A] {
        type TypeClassType = Decidable[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToDecidableOps
}
