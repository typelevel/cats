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

import cats.arrow.FunctionK
import cats.data.EitherK

/**
 * InjectK is a type class providing an injection from type
 * constructor `F` into type constructor `G`. An injection is a
 * functor transformation `inj` which does not destroy any
 * information: for every `ga: G[A]` there is at most one `fa: F[A]`
 * such that `inj(fa) = ga`.
 *
 * Because of this all injections admit partial inverses `prj` which
 * pair a value `ga: G[A]` back with a single value `fa: F[A]`.
 *
 * The behavior of the default instances for the InjectK type class
 * are described thoroughly in "Data types a la carte" (Swierstra
 * 2008).
 *
 * @note Prior to cats 1.0, InjectK was known as [[Inject]].
 *
 * @see [[http://www.staff.science.uu.nl/~swier004/publications/2008-jfp.pdf]]
 * @see [[Inject]] for injection for `Either`
 */
abstract class InjectK[F[_], G[_]] {
  def inj: FunctionK[F, G]

  def prj: FunctionK[G, λ[α => Option[F[α]]]]

  final def apply[A](fa: F[A]): G[A] = inj(fa)

  final def unapply[A](ga: G[A]): Option[F[A]] = prj(ga)
}

sealed abstract private[cats] class InjectKInstances {
  implicit def catsReflexiveInjectKInstance[F[_]]: InjectK[F, F] =
    new InjectK[F, F] {
      val inj = FunctionK.id[F]

      val prj: FunctionK[F, λ[α => Option[F[α]]]] = new FunctionK[F, λ[α => Option[F[α]]]] {
        def apply[A](a: F[A]): Option[F[A]] = Some(a)
      }
    }

  implicit def catsLeftInjectKInstance[F[_], G[_]]: InjectK[F, EitherK[F, G, *]] =
    new InjectK[F, EitherK[F, G, *]] {
      val inj: FunctionK[F, EitherK[F, G, *]] = new FunctionK[F, EitherK[F, G, *]] {
        def apply[A](a: F[A]): EitherK[F, G, A] = EitherK.leftc(a)
      }

      val prj: FunctionK[EitherK[F, G, *], λ[α => Option[F[α]]]] =
        new FunctionK[EitherK[F, G, *], λ[α => Option[F[α]]]] {
          def apply[A](a: EitherK[F, G, A]): Option[F[A]] = a.run.left.toOption
        }
    }

  implicit def catsRightInjectKInstance[F[_], G[_], H[_]](implicit I: InjectK[F, G]): InjectK[F, EitherK[H, G, *]] =
    new InjectK[F, EitherK[H, G, *]] {
      val inj = new FunctionK[G, EitherK[H, G, *]] { def apply[A](a: G[A]): EitherK[H, G, A] = EitherK.rightc(a) }
        .compose(I.inj)

      val prj: FunctionK[EitherK[H, G, *], λ[α => Option[F[α]]]] =
        new FunctionK[EitherK[H, G, *], λ[α => Option[F[α]]]] {
          def apply[A](a: EitherK[H, G, A]): Option[F[A]] = a.run.toOption.flatMap(I.prj(_))
        }
    }
}

object InjectK extends InjectKInstances {
  def apply[F[_], G[_]](implicit I: InjectK[F, G]): InjectK[F, G] = I
}
