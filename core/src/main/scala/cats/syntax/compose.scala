package cats
package syntax

import cats.arrow.Compose

trait ComposeSyntax {
  // TODO: use simulacrum instances eventually
  implicit def catsSyntaxCompose[F[_, _]: Compose, A, B](fab: F[A, B]): ComposeOps[F, A, B] =
    new ComposeOps[F, A, B](fab)
}

final class ComposeOps[F[_, _], A, B](fab: F[A, B])(implicit F: Compose[F]) {
  def compose[Z](fza: F[Z, A]): F[Z, B] = F.compose(fab, fza)
  def andThen[C](fbc: F[B, C]): F[A, C] = F.andThen(fab, fbc)
}
