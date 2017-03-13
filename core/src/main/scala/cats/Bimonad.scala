package cats

import simulacrum.typeclass

@typeclass trait Bimonad[F[_]] extends Monad[F] with Comonad[F]
