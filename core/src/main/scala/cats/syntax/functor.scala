package cats
package syntax

trait FunctorSyntax extends Functor.ToFunctorOps {
  implicit final def catsSyntaxFunctorTuple2Ops[F[_], A, B](fab: F[(A, B)]): FunctorTuple2Ops[F, A, B] =
    new FunctorTuple2Ops[F, A, B](fab)
}

final class FunctorTuple2Ops[F[_], A, B](private val fab: F[(A, B)]) extends AnyVal {

  /**
   * Lifts `Tuple2#_1` to Functor
   *
   * {{{
   * scala> import cats.data.Chain
   * scala> import cats.syntax.functor._
   *
   * scala> Chain((1, 2), (3, 4), (5, 6))._1F == Chain(1, 3, 5)
   * res0: Boolean = true
   * }}}
   */
  def _1F(implicit F: Functor[F]): F[A] = F.map(fab)(_._1)

  /**
   * Lifts `Tuple2#_2` to Functor
   *
   * {{{
   * scala> import cats.data.Chain
   * scala> import cats.syntax.functor._
   *
   * scala> Chain((1, 2), (3, 4), (5, 6))._2F == Chain(2, 4, 6)
   * res0: Boolean = true
   * }}}
   */
  def _2F(implicit F: Functor[F]): F[B] = F.map(fab)(_._2)

  /**
   * Lifts `Tuple2#swap` to Functor
   *
   * {{{
   * scala> import cats.data.Chain
   * scala> import cats.syntax.functor._
   *
   * scala> Chain((1, 2), (3, 4), (5, 6)).swapF == Chain((2, 1), (4, 3), (6, 5))
   * res0: Boolean = true
   * }}}
   */
  def swapF(implicit F: Functor[F]): F[(B, A)] = F.map(fab)(_.swap)

  /**
   * Un-zips an `F[(A, B)]` consisting of element pairs or Tuple2 into two separate F's tupled.
   *
   * NOTE: Check for effect duplication, possibly memoize before
   *
   * {{{
   * scala> import cats.data.Chain
   * scala> import cats.syntax.functor._
   *
   * scala> Chain((1, 2), (3, 4), (5, 6)).unzip == ((Chain(1, 3, 5), Chain(2, 4, 6)))
   * res0: Boolean = true
   * }}}
   */
  def unzip(implicit F: Functor[F]): (F[A], F[B]) = F.unzip(fab)
}
