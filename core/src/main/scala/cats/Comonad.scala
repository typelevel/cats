package cats

import simulacrum._

/**
 * Must obey the laws defined in [[laws.ComonadLaws]].
 */
@typeclass trait Comonad[F[_]] extends CoflatMap[F] {
  def extract[A](x: F[A]): A
}
