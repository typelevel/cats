package cats
package laws

/**
 * Laws that must be obeyed by any `Bimonad`.
 */
trait BimonadLaws[F[_]] extends MonadLaws[F] with ComonadLaws[F] {
  implicit override def F: Bimonad[F]

  def pureExtractIsId[A](a: A): IsEq[A] =
    F.extract(F.pure(a)) <-> a

  def extractPureIsId[A](fa: F[A]): IsEq[F[A]] =
    F.pure(F.extract(fa)) <-> fa
}

object BimonadLaws {
  def apply[F[_]](implicit ev: Bimonad[F]): BimonadLaws[F] =
    new BimonadLaws[F] { def F: Bimonad[F] = ev }
}
