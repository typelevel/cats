package cats

import simulacrum._

@typeclass trait MonadCombine[F[_]] extends MonadFilter[F] with MonoidK[F] {
  def unite[G[_]: Foldable, A](fga: F[G[A]]): F[A] =
    flatMap(fga) { ga =>
      Foldable[G].foldLeft(ga, empty[A])((acc, a) => combine(acc, pure(a)))
    }
}
