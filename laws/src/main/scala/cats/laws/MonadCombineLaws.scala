package cats.laws

import cats.{MonadCombine, Monad}

/**
 * Laws that must be obeyed by any [[MonadCombine]].
 */
trait MonadCombineLaws[F[_]] extends MonadFilterLaws[F] with MonoidKLaws[F] {
  implicit override def F: MonadCombine[F]
}

object MonadCombineLaws {
  def apply[F[_]](implicit ev: MonadCombine[F]): MonadCombineLaws[F] =
    new MonadCombineLaws[F] { def F = ev }
}
