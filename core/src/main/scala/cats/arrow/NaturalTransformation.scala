package cats
package arrow

trait NaturalTransformation[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}

object NaturalTransformation {
  def apply[F[_], G[_]](implicit ev: NaturalTransformation[F, G]): NaturalTransformation[F, G] = ev
}
