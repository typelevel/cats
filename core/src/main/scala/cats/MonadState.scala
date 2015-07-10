package cats

/** A monad that can read, update, and pass along state (e.g. `StateT`). */
trait MonadState[F[_, _], S] extends Monad[F[S, ?]] {
  def get: F[S, S]

  def set(s: S): F[S, Unit]

  def modify(f: S => S): F[S, Unit] = flatMap(get)(s => set(f(s)))
}

object MonadState {
  def apply[F[_, _], S](implicit F: MonadState[F, S]): MonadState[F, S] = F
}
