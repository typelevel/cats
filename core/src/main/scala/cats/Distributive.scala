package cats
import simulacrum.typeclass

@typeclass trait Distributive[F[_]] extends Functor[F] { self =>

  /**
   * Given a function which returns a distributive `F`, apply that value across the structure G.
   */
  def distribute[G[_]: Functor, A, B](ga: G[A])(f: A => F[B]): F[G[B]]

  /**
   * Given a Functor G which wraps some distributive F, distribute F across the G.
   */
  def cosequence[G[_]: Functor, A](ga: G[F[A]]): F[G[A]] = distribute(ga)(identity)

  // Distributive composes
  def compose[G[_]](implicit G0: Distributive[G]): Distributive[λ[α => F[G[α]]]] =
    new ComposedDistributive[F, G] {
      implicit def F = self
      implicit def G = G0
    }
}
