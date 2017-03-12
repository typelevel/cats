package cats
package laws

/**
 * Laws that must be obeyed by any `Bimonad`.
 *
 * For more information, see definition 4.1 from this paper:
 * http://arxiv.org/pdf/0710.1163v3.pdf
 */
trait BimonadLaws[F[_]] extends MonadLaws[F] with ComonadLaws[F] {
  implicit override def F: Bimonad[F]

  def pureExtractIsId[A](a: A): IsEq[A] =
    F.extract(F.pure(a)) <-> a

  def extractFlatMapEntwining[A](ffa: F[F[A]]): IsEq[A] =
    F.extract(F.flatten(ffa)) <-> F.extract(F.map(ffa)(F.extract))

  def pureCoflatMapEntwining[A](a: A): IsEq[F[F[A]]] =
    F.coflatten(F.pure(a)) <-> F.map(F.pure(a))(F.pure)
}

object BimonadLaws {
  def apply[F[_]](implicit ev: Bimonad[F]): BimonadLaws[F] =
    new BimonadLaws[F] { def F: Bimonad[F] = ev }
}
