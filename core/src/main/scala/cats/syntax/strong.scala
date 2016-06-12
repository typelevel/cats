package cats
package syntax

import cats.functor.Strong

trait StrongSyntax {
  // TODO: use simulacrum instances eventually
  implicit def catsSyntaxStrong[F[_, _]: Strong, A, B](fab: F[A, B]): StrongOps[F, A, B] =
    new StrongOps[F, A, B](fab)
}

final class StrongOps[F[_, _], A, B](fab: F[A, B])(implicit F: Strong[F]) {
  def first[C]: F[(A, C), (B, C)] = F.first(fab)
  def second[C]: F[(C, A), (C, B)] = F.second(fab)
}
