package cats
package arrow

trait NaturalTransformation[F[_], G[_]] extends Serializable {
  def apply[A](fa: F[A]): G[A]
}
