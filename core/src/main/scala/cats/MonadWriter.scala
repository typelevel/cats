package cats

/** A monad that support monoidal accumulation (e.g. logging List[String]) */
trait MonadWriter[F[_], W] extends Monad[F] {
  /** Lift a writer action into the effect */
  def writer[A](aw: (W, A)): F[A]

  /** Run the effect and pair the accumulator with the result */
  def listen[A](fa: F[A]): F[(W, A)]

  /** Apply the effectful function to the accumulator */
  def pass[A](fa: F[(W => W, A)]): F[A]

  /** Lift the log into the effect */
  def tell(w: W): F[Unit] = writer((w, ()))

  /** Pair the value with an inspection of the accumulator */
  def listens[A, B](fa: F[A])(f: W => B): F[(B, A)] =
    map(listen(fa)) { case (w, a) => (f(w), a) }

  /** Modify the accumulator */
  def censor[A](fa: F[A])(f: W => W): F[A] =
    flatMap(listen(fa)) { case (w, a) => writer((f(w), a)) }
}

object MonadWriter {
  def apply[F[_], W](implicit F: MonadWriter[F, W]): MonadWriter[F, W] = F
}
