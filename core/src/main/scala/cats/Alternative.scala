package cats

import simulacrum.typeclass

@typeclass trait Alternative[F[_]] extends Applicative[F] with MonoidK[F] { self =>
  override def compose[G[_]: Applicative]: Alternative[λ[α => F[G[α]]]] =
    new ComposedAlternative[F, G] {
      val F = self
      val G = Applicative[G]
    }
}
