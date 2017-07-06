package cats
package laws

/**
 * Laws that must be obeyed by any `CommutativeCoflatMap`.
 */
trait CommutativeCoflatMapLaws[F[_]] extends CoflatMapLaws[F] {
  implicit override def F: CommutativeCoflatMap[F]

  def coflatmapCommutative[A, B, C](fa: F[A], fb: F[B], g: (F[A], F[B]) => C): IsEq[F[F[C]]] =
    F.coflatMap(fa)(x => F.coflatMap(fb)(y => g(x, y))) <->
    F.coflatMap(fb)(y => F.coflatMap(fa)(x => g(x, y)))
}

object CommutativeCoflatMapLaws {
  def apply[F[_]](implicit ev: CommutativeCoflatMap[F]): CommutativeCoflatMapLaws[F] =
    new CommutativeCoflatMapLaws[F] { def F: CommutativeCoflatMap[F] = ev }
}
