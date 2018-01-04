package cats
package laws

trait InjectKLaws[F[_], G[_]] {
  def injectK: InjectK[F, G]

  def injectKRoundTripInj[A](fa: F[A]): IsEq[Option[F[A]]] =
    (injectK.prj compose injectK.inj).apply(fa) <-> Some(fa)

  def injectKRoundTripPrj[A](ga: G[A]): IsEq[Option[G[A]]] =
    injectK.prj(ga) match {
      case Some(fa) => (Some(injectK.inj(fa)): Option[G[A]]) <-> Some(ga)
      case None     => (None: Option[G[A]]) <-> None
    }
}

object InjectKLaws {
  def apply[F[_], G[_]](implicit ev: InjectK[F, G]): InjectKLaws[F, G] =
    new InjectKLaws[F, G]{ val injectK: InjectK[F, G] = ev }
}
