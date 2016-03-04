package cats.data

import cats.Functor
import cats.fix.Fix
import Types._

/**
  *
  * Polynomial Types
  *
  */

object Types {

  type Constant[Y, Z] = Y
  type Identity[Y, Z] = Z
  type Product[F[_, _], G[_, _], Y, Z] = Prod[λ[ψ => F[Y, ψ]], λ[ψ => G[Y, ψ]], Z]
  type Sum[F[_, _], G[_, _], Y, Z] = Xor[F[Y, Z], G[Y, Z]]

  type ConstantUnit[Y, Z] = Constant[Unit, Z]

}

/**
  *
  * Polynomial Functors
  *
  */

class ConstantFunctor[Y] extends Functor[λ[ζ => Y]] {
  def map[Z, X](y: Y)(z2x: Z => X): Y = y
}

class IdentityFunctor[Y] extends Functor[λ[ζ => ζ]] {
  def map[Z, X](z: Z)(z2x: Z => X): X = z2x(z)
}

class ProductFunctor[F[_, _], G[_, _], Y](F: Functor[λ[ζ => F[Y, ζ]]], G: Functor[λ[ζ => G[Y, ζ]]]) extends Functor[λ[ζ => Product[F, G, Y, ζ]]] {
  def map[Z, X](prod: Product[F, G, Y, Z])(z2x: Z => X): Product[F, G, Y, X] =
    prod.bimap(F.map(_)(z2x), G.map(_)(z2x))
}


class SumFunctor[F[_, _], G[_, _], Y](F: Functor[λ[ζ => F[Y, ζ]]], G: Functor[λ[ζ => G[Y, ζ]]]) extends Functor[λ[ζ => Sum[F, G, Y, ζ]]] {
  def map[Z, X](xor: Sum[F, G, Y, Z])(z2x: Z => X): Sum[F, G, Y, X] =
    xor.bimap(F.map(_)(z2x), G.map(_)(z2x))
}

