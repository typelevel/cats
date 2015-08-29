package cats

import simulacrum.typeclass

@typeclass trait Alternative[F[_]] extends Applicative[F] with MonoidK[F]

