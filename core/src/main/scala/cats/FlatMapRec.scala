package cats

import simulacrum.typeclass

/**
 * Version of [[cats.FlatMap]] capable of stack-safe recursive `flatMap`s.
 *
 * Based on Phil Freeman's
 * [[http://functorial.com/stack-safety-for-free/index.pdf Stack Safety for Free]].
 */
@typeclass trait FlatMapRec[F[_]] extends FlatMap[F] {

  /**
   * Keeps calling `f` until a `[[scala.util.Right]][B]` is returned.
   *
   * Implementations of this method must use constant stack space.
   *
   * `f` must use constant stack space. (It is OK to use a constant number of
   * `map`s and `flatMap`s inside `f`.)
   */
  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B]
}
