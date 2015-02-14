package cats

import simulacrum._

trait Bimonad[F[_]] extends Monad[F] with Comonad[F]

object Bimonad {
  def apply[F[_]](implicit ev: Bimonad[F]): Bimonad[F] = ev
}
