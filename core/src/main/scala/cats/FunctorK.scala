package cats

import arrow.FunctionK

import simulacrum.typeclass


@typeclass trait FunctorK[F[_[_]]] {
  def mapK[G[_], H[_]](fg: F[G])(f: FunctionK[G, H]): F[H]
}
