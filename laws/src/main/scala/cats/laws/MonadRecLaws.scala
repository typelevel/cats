package cats
package laws

/**
 * Laws that must be obeyed by any `MonadRec`.
 */
trait MonadRecLaws[F[_]] extends MonadLaws[F] with FlatMapRecLaws[F] {
  implicit override def F: MonadRec[F]
}

object MonadRecLaws {
  def apply[F[_]](implicit ev: MonadRec[F]): MonadRecLaws[F] =
    new MonadRecLaws[F] { def F: MonadRec[F] = ev }
}
