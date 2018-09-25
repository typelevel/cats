package alleycats

import simulacrum.op
import simulacrum.typeclass

@typeclass trait OrElse[F[_]] {
  @op("orElse") def orElse[A](fa: F[A], alternative: => F[A]): F[A]
}
