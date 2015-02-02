package cats
package arrow

trait NaturalTransformation[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}

object NaturalTransformation {
}
