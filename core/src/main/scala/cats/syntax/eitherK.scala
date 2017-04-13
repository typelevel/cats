package cats
package syntax

import cats.data.EitherK

trait EitherKSyntax {
  implicit final def catsSyntaxEitherK[F[_], A](a: F[A]): EitherKOps[F, A] = new EitherKOps(a)
}

final class EitherKOps[F[_], A](val fa: F[A]) extends AnyVal {

  /**
   * Lift an `F[A]` into a `EitherK[F, G, A]` for any type constructor `G[_]`.
   *
   * @see [[rightc]] to swap the order of `F` and `G` in the result type.
   *
   * Example:
   * {{{
   * scala> import cats.data.EitherK
   * scala> import cats.Eval
   * scala> import cats.implicits._
   * scala> List(1, 2, 3).leftc[Eval]
   * res0: EitherK[List, Eval, Int] = EitherK(Left(List(1, 2, 3)))
   * }}}
   */
  def leftc[G[_]]: EitherK[F, G, A] = EitherK.leftc(fa)

  /**
   * Lift an `F[A]` into a `EitherK[G, F, A]` for any type constructor `G[_]`.
   *
   * @see [[leftc]] to swap the order of `F` and `G` in the result type.
   *
   * Example:
   * {{{
   * scala> import cats.data.EitherK
   * scala> import cats.Eval
   * scala> import cats.implicits._
   * scala> List(1, 2, 3).rightc[Eval]
   * res0: EitherK[Eval, List, Int] = EitherK(Right(List(1, 2, 3)))
   * }}}
   */
  def rightc[G[_]]: EitherK[G, F, A] = EitherK.rightc(fa)
}
