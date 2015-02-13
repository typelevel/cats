package cats
package functor

trait Bifunctor[F[_, _]] { self =>
  def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]
}

object Bifunctor {
  def apply[F[_, _]](implicit ev: Bifunctor[F]): Bifunctor[F] = ev
}
