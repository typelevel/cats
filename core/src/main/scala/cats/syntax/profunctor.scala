package cats
package syntax

import cats.functor.Profunctor

trait ProfunctorSyntax {
  // TODO: use simulacrum instances eventually
  implicit def catsSyntaxProfunctor[F[_, _]: Profunctor, A, B](fab: F[A, B]): ProfunctorOps[F, A, B] =
    new ProfunctorOps[F, A, B](fab)
}

final class ProfunctorOps[F[_, _], A, B](fab: F[A, B])(implicit F: Profunctor[F]) {
  def lmap[C](f: C => A): F[C, B] = F.lmap(fab)(f)
  def rmap[C](f: B => C): F[A, C] = F.rmap(fab)(f)
  def dimap[C, D](f: C => A)(g: B => D): F[C, D] = F.dimap(fab)(f)(g)
}
