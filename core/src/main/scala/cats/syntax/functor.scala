package cats
package syntax

import scala.language.implicitConversions

trait FunctorSyntax extends Functor.ToFunctorOps

private[syntax] trait FunctorSyntaxBinCompat0 {
  implicit final def catsSyntaxUnzipFunctorOps[F[_], A, B](fa: F[(A, B)]): UnzipFunctorOps[F, A, B] =
    new UnzipFunctorOps[F, A, B](fa)
}

final class UnzipFunctorOps[F[_], A, B](private val fab: F[(A, B)]) extends AnyVal {

  /**
   * Un-zips an `F[(A, B)]` consisting of element pairs or Tuple2 into two separate F's tupled.
   *
   * NOTE: Check for effect duplication, possibly memoize before
   *
   * {{{
   * scala> import cats.Id
   * scala> import cats.syntax.functor._
   *
   * scala> (5: Id[Int]).map(i => (i, i)).unzip == ((5, 5))
   * res0: Boolean = true
   * }}}
   *
   */
  def unzip(implicit F: Functor[F]): (F[A], F[B]) = (F.map(fab)(_._1), F.map(fab)(_._2))
}
