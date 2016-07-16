package cats

/** A monad that can read, update, and pass along state (e.g. `StateT`).
 *
 * A common use case for `MonadState` is for syntax, especially when
 * dealing with large monad transformer stacks. For instance:
 *
 * {{{
 * val M = MonadState[StateT[List, Int, ?], Int]
 * import M._
 *
 * for {
 *   g <- get
 *   _ <- set(g + 1)
 *   r <- inspect(_ * 100)
 * } yield r
 * }}}
 */
trait MonadState[F[_], S] extends Monad[F] {
  def get: F[S]

  def set(s: S): F[Unit]

  def modify(f: S => S): F[Unit] = flatMap(get)(s => set(f(s)))

  def inspect[A](f: S => A): F[A] = map(get)(f)
}

object MonadState {
  def apply[F[_], S](implicit F: MonadState[F, S]): MonadState[F, S] = F
}
