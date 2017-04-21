package cats
package laws

import arrow.FunctionK
import syntax.all._

trait FunctorKLaws[F[_[_]]] {
  implicit def F: FunctorK[F]

  def covariantIdentity[G[_]](fg: F[G]): IsEq[F[G]] =
    fg.mapK(FunctionK.id[G]) <-> fg

  def covariantComposition[G[_], H[_], I[_]](fg: F[G], f: FunctionK[G, H], g: FunctionK[H, I]): IsEq[F[I]] =
    fg.mapK(f).mapK(g) <-> fg.mapK(f andThen g)

}

object FunctorKLaws {
  def apply[F[_[_]]](implicit ev: FunctorK[F]): FunctorKLaws[F] =
    new FunctorKLaws[F] { def F: FunctorK[F] = ev }
}
