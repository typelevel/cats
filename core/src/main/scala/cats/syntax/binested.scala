package cats
package syntax

import cats.data.Binested

trait BinestedSyntax {
  implicit final def catsSyntaxBinestedId[F[_, _], G[_], H[_], A, B](value: F[G[A], H[B]]): BinestedIdOps[F, G, H, A, B] =
    new BinestedIdOps(value)
}

final class BinestedIdOps[F[_, _], G[_], H[_], A, B](val value: F[G[A], H[B]]) extends AnyVal {
  def binested: Binested[F, G, H, A, B] = Binested(value)
}
