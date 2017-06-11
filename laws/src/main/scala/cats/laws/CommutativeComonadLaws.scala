package cats
package laws

/**
 * Laws that must be obeyed by any `CommutativeComonad`.
 */
trait CommutativeComonadLaws[F[_]] extends ComonadLaws[F] with CommutativeCoflatMapLaws[F] {
  implicit override def F: CommutativeComonad[F]
}

object CommutativeComonadLaws {
  def apply[F[_]](implicit ev: CommutativeComonad[F]): CommutativeComonadLaws[F] =
    new CommutativeComonadLaws[F] { def F: CommutativeComonad[F] = ev }
}
