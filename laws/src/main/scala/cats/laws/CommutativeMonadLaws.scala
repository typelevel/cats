package cats
package laws

/**
 * Laws that must be obeyed by any `CommutativeMonad`.
 */
trait CommutativeMonadLaws[F[_]] extends MonadLaws[F] {
  implicit override def F: CommutativeMonad[F]

  def monadCommutative[A, B, C](fa: F[A], fb: F[B], g: (A, B) => F[C]): IsEq[F[C]] =
    F.flatMap(fa)( a => F.flatMap(fb)( b => g(a, b))) <->
    F.flatMap(fb)( b => F.flatMap(fa)( a => g(a, b)))

}

object CommutativeMonadLaws {
  def apply[F[_]](implicit ev: CommutativeMonad[F]): CommutativeMonadLaws[F] =
    new CommutativeMonadLaws[F] { def F: CommutativeMonad[F] = ev }
}
