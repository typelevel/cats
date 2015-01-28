package cats

import simulacrum._

@typeclass trait Comonad[F[_]] extends CoFlatMap[F] {
  def extract[A](x: F[A]): A
}
