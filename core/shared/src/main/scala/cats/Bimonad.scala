package cats

import simulacrum._

@typeclass trait Bimonad[F[_]] extends Monad[F] with Comonad[F]
