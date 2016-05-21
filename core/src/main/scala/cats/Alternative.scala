package cats

import cats.data.NestedAlternative

import simulacrum.typeclass

@typeclass trait Alternative[F[_]] extends Applicative[F] with MonoidK[F] { self =>
  override def nest[G[_]: Applicative]: Alternative[Lambda[A => F[G[A]]]] =
    new NestedAlternative[F, G] {
      val F = self
      val G = Applicative[G]
    }
}
