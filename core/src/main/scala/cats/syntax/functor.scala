package cats
package syntax

import cats.evidence.<~<

trait FunctorSyntax extends Functor.ToFunctorOps {
  implicit final def catsSyntaxFunctorOps[F[_], A](fa: F[A]): FunctorOps[F, A] =
    new FunctorOps[F, A](fa)
}

final class FunctorOps[F[_], A](private val fa: F[A]) extends AnyVal {

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
  def unzip[X, Y](implicit F: Functor[F], ev: A <~< (X, Y)): (F[X], F[Y]) = F.unzip(F.map(fa)(ev))
}
