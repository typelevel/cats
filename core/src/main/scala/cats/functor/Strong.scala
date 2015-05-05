package cats
package functor

/**
 * Must obey the laws defined in cats.laws.StrongLaws.
 */
trait Strong[F[_, _]] extends Profunctor[F] {
  def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)]
  def second[A, B, C](fa: F[A, B]): F[(C, A), (C, B)]
}

object Strong {
  def apply[F[_, _]](implicit ev: Strong[F]): Strong[F] = ev
}
