package cats

import simulacrum._

trait Monad[F[_]] extends FlatMap[F] with Applicative[F] {
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))
}

object Monad {
  def apply[F[_]](implicit ev: Monad[F]): Monad[F] = ev
}
