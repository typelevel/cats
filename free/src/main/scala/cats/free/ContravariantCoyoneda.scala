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
package free

import cats.data.AndThen

/**
 * The free contravariant functor on `F`. This is isomorphic to `F` as long as `F` itself is a
 * contravariant functor. The function from `F[A]` to `ContravariantCoyoneda[F,A]` exists even when
 * `F` is not a contravariant functor. Implemented using a List of functions for stack-safety.
 */
sealed abstract class ContravariantCoyoneda[F[_], A] extends Serializable { self =>
  import ContravariantCoyoneda.Aux

  /**
   * The pivot between `fi` and `k`, usually existential.
   */
  type Pivot

  /**
   * The underlying value.
   */
  val fi: F[Pivot]

  /**
   * The composed transformer function, to be lifted into `F` by `run`.
   */
  def k: A => Pivot

  /**
   * Converts to `F[A]` given that `F` is a contravariant functor
   */
  final def run(implicit F: Contravariant[F]): F[A] = F.contramap(fi)(k)

  /**
   * Converts to `G[A]` given that `G` is a contravariant functor
   */
  final def foldMap[G[_]](trans: F ~> G)(implicit G: Contravariant[G]): G[A] =
    G.contramap(trans(fi))(k)

  /**
   * Simple function composition. Allows contramap fusion without touching the underlying `F`.
   */
  final def contramap[B](f: B => A): Aux[F, B, Pivot] =
    ContravariantCoyoneda(fi)(AndThen(k).compose(f))

  /**
   * Modify the context `F` using transformation `f`.
   */
  final def mapK[G[_]](f: F ~> G): Aux[G, A, Pivot] =
    ContravariantCoyoneda(f(fi))(k)

}

object ContravariantCoyoneda {

  /**
   * Lift the `Pivot` type member to a parameter. It is usually more convenient to use `Aux` than
   * a refinment type.
   */
  type Aux[F[_], A, B] = ContravariantCoyoneda[F, A] { type Pivot = B }

  /**
   * `F[A]` converts to `ContravariantCoyoneda[F,A]` for any `F`
   */
  def lift[F[_], A](fa: F[A]): Aux[F, A, A] =
    apply(fa)(identity[A])

  /**
   * Like `lift(fa).contramap(k0)`.
   */
  def apply[F[_], A, B](fa: F[A])(k0: B => A): Aux[F, B, A] =
    new ContravariantCoyoneda[F, B] {
      type Pivot = A
      val k = k0
      val fi = fa
    }

  /**
   * `ContravariantCoyoneda[F, *]` provides a contravariant functor for any `F`.
   */
  implicit def catsFreeContravariantFunctorForContravariantCoyoneda[F[_]]: Contravariant[ContravariantCoyoneda[F, *]] =
    new Contravariant[ContravariantCoyoneda[F, *]] {
      def contramap[A, B](cfa: ContravariantCoyoneda[F, A])(f: B => A): ContravariantCoyoneda[F, B] =
        cfa.contramap(f)
    }

}
