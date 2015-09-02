package cats
package laws

/**
 * Laws that must be obeyed by any `Bimonad`.
 */
trait BimonadLaws[F[_]] extends MonadLaws[F] with ComonadLaws[F] {
  implicit override def F: Bimonad[F]

  def pureExtractComposition[A](a: A): IsEq[A] =
    F.extract(F.pure(a)) <-> a
}

object BimonadLaws {
  def apply[F[_]](implicit ev: Bimonad[F]): BimonadLaws[F] =
    new BimonadLaws[F] { def F: Bimonad[F] = ev }
}
