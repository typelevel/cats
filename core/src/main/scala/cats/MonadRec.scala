package cats

import simulacrum.typeclass

@typeclass trait MonadRec[F[_]] extends Monad[F] with FlatMapRec[F]
