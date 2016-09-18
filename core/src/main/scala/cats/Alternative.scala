package cats

import simulacrum.typeclass

@typeclass trait Alternative[F[_]] extends MonoidK[F] { self =>
  def applicativeInstance: Applicative[F]

  def compose[G[_]: Applicative]: Alternative[λ[α => F[G[α]]]] =
    new ComposedAlternative[F, G] {
      val F = self
      val G = Applicative[G]
    }
}
