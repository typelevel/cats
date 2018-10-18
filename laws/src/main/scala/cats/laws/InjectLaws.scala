package cats
package laws

trait InjectLaws[A, B] {
  def inject: Inject[A, B]

  def injectRoundTripInj(a: A): IsEq[Option[A]] =
    inject.prj.compose(inject.inj).apply(a) <-> Some(a)

  def injectRoundTripPrj(b: B): IsEq[Option[B]] =
    inject.prj(b) match {
      case Some(a) => (Some(inject.inj(a)): Option[B]) <-> Some(b)
      case None    => (None: Option[B]) <-> None
    }
}

object InjectLaws {
  def apply[A, B](implicit ev: Inject[A, B]): InjectLaws[A, B] =
    new InjectLaws[A, B] { val inject: Inject[A, B] = ev }
}
