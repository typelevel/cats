package cats.kernel

trait Delay[F[_], G[_]] {
  def apply[A](fa: F[A]): F[G[A]]
}

object Delay {
  def apply[F[_], G[_]](implicit D: Delay[F, G]): Delay[F, G] = D
}
