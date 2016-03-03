package cats

/** A monad that support monoidal accumulation (e.g. logging List[String]) */
trait MonadWriter[F[_], W] extends Monad[F] {
  /** Lift a writer action into the effect */
  def writer[A](aw: (A, W)): F[A]

  /** Run the effect and pair the accumulator with the result */
  def listen[A](fa: F[A]): F[(A, W)]

  /** Apply the effectful function to the accumulator */
  def pass[A](fa: F[(A, W => W)]): F[A]

  /** An effect that when run, logs w */
  def tell(w: W): F[Unit] = writer(((), w))
}

object MonadWriter {
  def apply[F[_], W](implicit F: MonadWriter[F, W]): MonadWriter[F, W] = F
}
