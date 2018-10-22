package cats

import simulacrum.typeclass

/**
 * Comonad
 *
 * Comonad is the dual of Monad. Whereas Monads allow for the composition of effectful functions,
 * Comonads allow for composition of functions that extract the value from their context.
 *
 * Must obey the laws defined in cats.laws.ComonadLaws.
 */
@typeclass trait Comonad[F[_]] extends CoflatMap[F] {

  /**
   * `extract` is the dual of `pure` on Monad (via `Applicative`)
   * and extracts the value from its context
   *
   * Example:
   * {{{
   * scala> import cats.Id
   * scala> import cats.Comonad
   * scala> val id: Id[Int] = 3
   * scala> Comonad[Id].extract(id)
   * res0: cats.Id[Int] = 3
   * }}}
   */
  def extract[A](x: F[A]): A
}
