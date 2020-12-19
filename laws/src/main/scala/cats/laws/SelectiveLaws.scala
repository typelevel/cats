package cats
package laws

/**
 * Laws that must be obeyed by any `Selective`.
 */
trait SelectiveLaws[F[_]] extends ApplicativeLaws[F] {
  implicit override def F: Selective[F]
}

object SelectiveLaws {
  def apply[F[_]](implicit ev: Selective[F]): SelectiveLaws[F] =
    new SelectiveLaws[F] { def F: Selective[F] = ev }
}
