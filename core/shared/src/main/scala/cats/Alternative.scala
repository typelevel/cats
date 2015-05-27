package cats

import simulacrum._

@typeclass trait Alternative[F[_]] extends Applicative[F] with MonoidK[F]

