package cats
package syntax

import cats.evidence.===

trait DistributiveSyntax extends Distributive.ToDistributiveOps {
  implicit final def catsSyntaxDistributiveOps[F[_]: Functor, A](fa: F[A]): DistributiveOps[F, A] =
    new DistributiveOps[F, A](fa)
}

// Add syntax to functor as part of importing distributive syntax.
final class DistributiveOps[F[_], A](private val fa: F[A]) extends AnyVal {
  def distribute[G[_], B](f: A => G[B])(implicit G: Distributive[G], F: Functor[F]): G[F[B]] = G.distribute(fa)(f)
  def cosequence[G[_], B](implicit G: Distributive[G], F: Functor[F], ev: A === G[B]): G[F[B]] =
    G.cosequence(ev.substitute(fa))
}
