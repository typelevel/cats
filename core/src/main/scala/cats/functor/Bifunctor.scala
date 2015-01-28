package cats
package functor

trait Bifunctor[F[_, _]] { self =>
  def bimap[A, B, C, D](fab: F[A, B], fcd: F[C, D]): F[(A, C), (B, D)]
}

object Bifunctor {
  def apply[F[_, _]](implicit ev: Bifunctor[F]): Bifunctor[F] = ev
}
