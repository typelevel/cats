package cats
package syntax

import cats.data.Coproduct

trait CoproductSyntax {
  implicit def catsSyntaxCoproduct[F[_], A](a: F[A]): CoproductOps[F, A] = new CoproductOps(a)
}

final class CoproductOps[F[_], A](val fa: F[A]) extends AnyVal {

  /**
   * Lift an `F[A]` into a `Coproduct[F, G, A]` for any type constructor `G[_]`.
   *
   * @see [[rightc]] to swap the order of `F` and `G` in the result type.
   *
   * Example:
   * {{{
   * scala> import cats.data.Coproduct
   * scala> import cats.Eval
   * scala> import cats.implicits._
   * scala> List(1, 2, 3).leftc[Eval]
   * res0: Coproduct[List, Eval, Int] = Coproduct(Left(List(1, 2, 3)))
   * }}}
   */
  def leftc[G[_]]: Coproduct[F, G, A] = Coproduct.leftc(fa)

  /**
   * Lift an `F[A]` into a `Coproduct[G, F, A]` for any type constructor `G[_]`.
   *
   * @see [[leftc]] to swap the order of `F` and `G` in the result type.
   *
   * Example:
   * {{{
   * scala> import cats.data.Coproduct
   * scala> import cats.Eval
   * scala> import cats.implicits._
   * scala> List(1, 2, 3).rightc[Eval]
   * res0: Coproduct[Eval, List, Int] = Coproduct(Right(List(1, 2, 3)))
   * }}}
   */
  def rightc[G[_]]: Coproduct[G, F, A] = Coproduct.rightc(fa)
}
