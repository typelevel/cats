package cats

/** A monad that has the ability to read from an environment. */
trait MonadReader[F[_, _], R] extends Monad[F[R, ?]] {
  /** Get the environment */
  def ask: F[R, R]

  /** Modify the environment */
  def local[A](f: R => R)(fa: F[R, A]): F[R, A]
}

object MonadReader {
  def apply[F[_, _], R](implicit F: MonadReader[F, R]): MonadReader[F, R] = F
}
