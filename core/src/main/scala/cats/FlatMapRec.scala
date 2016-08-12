package cats

import simulacrum.typeclass

import cats.data.Xor

/**
 * Version of [[cats.FlatMap]] capable of stack-safe recursive `flatMap`s.
 *
 * Based on Phil Freeman's
 * [[http://functorial.com/stack-safety-for-free/index.pdf Stack Safety for Free]].
 *
 * This Scala implementation of `FlatMapRec` and its usages are derived from
 * [[https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/BindRec.scala Scalaz's BindRec]],
 * originally written by Brian McKenna.
 */
@typeclass trait FlatMapRec[F[_]] extends FlatMap[F] {

  /**
   * Keeps calling `f` until a `[[cats.data.Xor.Right Right]][B]` is returned.
   *
   * Implementations of this method must use constant stack space.
   *
   * `f` must use constant stack space. (It is OK to use a constant number of
   * `map`s and `flatMap`s inside `f`.)
   */
  def tailRecM[A, B](a: A)(f: A => F[A Xor B]): F[B]
}
