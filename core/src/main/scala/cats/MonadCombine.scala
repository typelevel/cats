package cats

import simulacrum._

trait MonadCombine[F[_]] extends MonadFilter[F] with MonoidK[F] with Alternative[F] {
  def unite[G[_]: Foldable, A](fga: F[G[A]]): F[A] =
    flatMap(fga) { ga =>
      Foldable[G].foldLeft(ga, empty[A])((acc, a) => combine(acc, pure(a)))
    }
}

object MonadCombine {
  def apply[F[_]](implicit ev: MonadCombine[F]): MonadCombine[F] = ev
}
