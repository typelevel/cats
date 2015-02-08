package cats
package syntax

import cats.functor.Bifunctor

trait BifunctorSyntax {
  // TODO: use simulacrum instances eventually
  implicit def bifunctorSyntax[F[_, _]: Bifunctor, A, B](fab: F[A, B]): BifunctorOps[F, A, B] =
    new BifunctorOps[F, A, B](fab)
}

class BifunctorOps[F[_, _], A, B](fab: F[A, B])(implicit F: Bifunctor[F]) {
  def bimap[C, D](fcd: F[C, D]): F[(A, C), (B, D)] = F.bimap(fab, fcd)
}
