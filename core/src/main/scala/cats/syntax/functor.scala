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
   * scala> import cats.instances.option._
   * scala> import cats.syntax.option._
   * scala> import cats.syntax.functor._
   *
   * scala> (1, 2).some._1F == Some(1)
   * res0: Boolean = true
   * }}}
   */
  def _1F(implicit F: Functor[F]): F[A] = F.map(fab)(_._1)

  /**
   * Lifts `Tuple2#_2` to Functor
   *
   * {{{
   * scala> import cats.instances.option._
   * scala> import cats.syntax.option._
   * scala> import cats.syntax.functor._
   *
   * scala> (1, 2).some._2F == Some(2)
   * res0: Boolean = true
   * }}}
   */
  def _2F(implicit F: Functor[F]): F[B] = F.map(fab)(_._2)

  /**
   * Lifts `Tuple2#swap` to Functor
   *
   * {{{
   * scala> import cats.instances.option._
   * scala> import cats.syntax.option._
   * scala> import cats.syntax.functor._
   *
   * scala> (1, 2).some.swapF == Some((2, 1))
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
   * scala> import cats.instances.option._
   * scala> import cats.syntax.option._
   * scala> import cats.syntax.functor._
   *
   * scala> 5.some.map(i => (i, i)).unzip == ((Some(5), Some(5)))
   * res0: Boolean = true
   * }}}
   */
  def unzip(implicit F: Functor[F]): (F[A], F[B]) = F.unzip(fab)
}
