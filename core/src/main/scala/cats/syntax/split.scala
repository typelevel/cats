package cats
package syntax

import cats.arrow.Split

trait SplitSyntax {
  // TODO: use simulacrum instances eventually
  implicit def catsSyntaxSplit[F[_, _]: Split, A, B](fab: F[A, B]): SplitOps[F, A, B] =
    new SplitOps[F, A, B](fab)
}

final class SplitOps[F[_, _], A, B](fab: F[A, B])(implicit F: Split[F]) {
  def split[C, D](fcd: F[C, D]): F[(A, C), (B, D)] = F.split(fab, fcd)
}
