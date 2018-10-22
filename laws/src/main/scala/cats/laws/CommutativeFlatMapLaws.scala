package cats
package laws

/**
 * Laws that must be obeyed by any `CommutativeFlatMap`.
 */
trait CommutativeFlatMapLaws[F[_]] extends CommutativeApplyLaws[F] with FlatMapLaws[F] {
  implicit override def F: CommutativeFlatMap[F]

  def flatmapCommutative[A, B, C](fa: F[A], fb: F[B], g: (A, B) => F[C]): IsEq[F[C]] =
    F.flatMap(fa)(a => F.flatMap(fb)(b => g(a, b))) <->
      F.flatMap(fb)(b => F.flatMap(fa)(a => g(a, b)))

}

object CommutativeFlatMapLaws {
  def apply[F[_]](implicit ev: CommutativeFlatMap[F]): CommutativeFlatMapLaws[F] =
    new CommutativeFlatMapLaws[F] { def F: CommutativeFlatMap[F] = ev }
}
