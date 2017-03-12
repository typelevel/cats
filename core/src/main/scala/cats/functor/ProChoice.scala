package cats.functor


trait ProChoice[F[_, _]] extends Profunctor[F] {
  def left[A, B, C](fab: F[A, B]): F[Either[A, C], Either[B, C]]
  def right[A, B, C](fab: F[A, B]): F[Either[C, A], Either[C, B]]
}

object ProChoice {
  def apply[F[_, _]](implicit ev: ProChoice[F]): ProChoice[F] = ev
}
