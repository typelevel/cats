package cats
package arrow

import cats.data.Xor

trait Choice[F[_, _]] extends Category[F] {
  def choice[A, B, C](f: F[A, C], g: F[B, C]): F[Xor[A, B], C]

  def codiagonal[A]: F[Xor[A, A], A] = choice(id, id)
}

object Choice {
  def apply[F[_, _]](implicit F: Choice[F]): Choice[F] = F
}
