package cats

import simulacrum._

trait FlatMap[F[_]] extends Apply[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def flatten[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(fa => fa)

  override def apply[A, B](fa: F[A])(ff: F[A => B]): F[B] =
    flatMap(ff)(f => map(fa)(f))
}

object FlatMap {
  def apply[F[_]](implicit ev: FlatMap[F]): FlatMap[F] = ev
}
