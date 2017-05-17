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

  /**
   * Embed a state action into the monad.
   *
   * Example:
   * {{{
   * scala> import cats.MonadState
   * scala> import cats.data.StateT
   * scala> import cats.instances.list._
   *
   * scala> val M = MonadState[StateT[List, Int, ?], Int]
   * scala> import M._
   *
   * scala> val st: StateT[List, Int, Int] = state(s => (s + 1, s * 100))
   * scala> st.run(1)
   * res0: List[(Int, Int)] = List((2,100))
   * }}}
   */
  def state[A](f: S => (S, A)): F[A] =
    flatMap(get)(s => f(s) match { case (s, a) => map(set(s))(_ => a) })

  def get: F[S]

  def set(s: S): F[Unit]

  def modify(f: S => S): F[Unit] = flatMap(get)(s => set(f(s)))

  def inspect[A](f: S => A): F[A] = map(get)(f)
}

object MonadState {
  def apply[F[_], S](implicit F: MonadState[F, S]): MonadState[F, S] = F
}
