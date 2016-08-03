package cats
package syntax

import cats.functor.Bifunctor

trait BifunctorSyntax {
  // TODO: use simulacrum instances eventually
  implicit def catsSyntaxBifunctor[F[_, _]: Bifunctor, A, B](fab: F[A, B]): BifunctorOps[F, A, B] =
    new BifunctorOps[F, A, B](fab)
}

final class BifunctorOps[F[_, _], A, B](fab: F[A, B])(implicit F: Bifunctor[F]) {
  def bimap[C, D](f: A => C, g: B => D): F[C, D] = F.bimap(fab)(f, g)

  def leftMap[C](f: A => C): F[C, B] = F.leftMap(fab)(f)

  def leftWiden[AA >: A]: F[AA, B] = F.leftWiden(fab)
}
