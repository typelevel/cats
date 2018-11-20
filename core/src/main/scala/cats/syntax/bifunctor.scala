package cats
package syntax

import cats.Bifunctor

trait BifunctorSyntax extends Bifunctor.ToBifunctorOps {
  implicit final def catsSyntaxBifunctor[F[_, _]: Bifunctor, A, B](fab: F[A, B]): BifunctorOps[F, A, B] =
    new BifunctorOps[F, A, B](fab)
}

final class BifunctorOps[F[_,_], A, B](private val fab: F[A,B]) extends AnyVal {

  def bimap[C, D](f: A => C, g: B => D)(implicit F: Bifunctor[F]): F[C, D] = F.bimap(fab)(f, g)

  def leftMap[C](f: A => C)(implicit F: Bifunctor[F]): F[C, B] = F.bimap(fab)(f, identity)

  def rightMap[D](g: A => D)(implicit F: Bifunctor[F]): F[A, D] = F.bimap(fab)(identity, g)

  def leftWiden[AA >: A]: F[AA, B] = fab.asInstanceOf[F[AA, B]]

  def rightWiden[BB >: B]: F[A, BB] = fab.asInstanceOf[F[A, BB]]

  def lift[C, D](f: A => C, g: B => D)(implicit F: Bifunctor[F]): F[A, B] => F[C, D] = F.bimap(_)(f, g)
}
