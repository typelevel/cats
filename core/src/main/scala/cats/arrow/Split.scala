package cats
package arrow

trait Split[F[_, _]] extends Compose[F] { self =>
  def split[A, B, C, D](f: F[A, B], g: F[C, D]): F[(A, C), (B, D)]
}

object Split {
  def apply[F[_, _]](implicit ev: Split[F]): Split[F] = ev
}
