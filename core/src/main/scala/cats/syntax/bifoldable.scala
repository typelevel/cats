package cats
package syntax

import scala.language.implicitConversions

trait BifoldableSyntax extends Bifoldable.ToBifoldableOps

private[syntax] trait BiFoldableSyntaxBinCompat0 {
  implicit final def catsSyntaxBiFoldOps[F[_, _], A, B](fa: F[A, B]): BifoldBifoldableOps[F, A, B] =
    new BifoldBifoldableOps[F, A, B](fa)
}

final class BifoldBifoldableOps[F[_, _], A, B](private val fab: F[A, B]) extends AnyVal {

  /**
   * Collapse the structure to a tuple2, given each type has an available [[cats.Monoid]]
   *
   * {{{
   * scala> import cats.implicits._
   * scala> Either.left[Int, String](5).bifold == ((5, ""))
   * res0: Boolean = true
   * }}}
   *
   */
  def bifold(implicit F: Bifoldable[F], A: Monoid[A], B: Monoid[B]): (A, B) = {
    import cats.instances.tuple._
    F.bifoldMap(fab)((_, B.empty), (A.empty, _))
  }
}
