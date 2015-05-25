package cats.functor

import cats.data.Xor


trait ProChoice[F[_, _]] extends Profunctor[F] {
  def left[A, B, C]( fab: F[A, B]): F[A Xor C, B Xor C]
  def right[A, B, C](fab: F[A, B]): F[C Xor A, C Xor B]
}

object ProChoice {
  def apply[F[_, _]](implicit ev: ProChoice[F]): ProChoice[F] = ev
}
