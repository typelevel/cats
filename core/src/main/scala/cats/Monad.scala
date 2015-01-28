package cats

import simulacrum._

@typeclass trait Monad[F[_]] extends FlatMap[F] with Applicative[F]
