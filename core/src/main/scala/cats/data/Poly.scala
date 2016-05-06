package cats.data

import cats.Functor
import Types._

/**
  *
  * Polynomial Types can, recursively, be defined in terms of
  *
  *  `Constant`
  *  `Identity`
  *  `Product`
  *  `Sum`
  *
  */

object Types {

  type Constant[Y, Z] = Y
  type Identity[Y, Z] = Z
  type Product[F[_, _], G[_, _], Y, Z] = Prod[λ[ψ => F[Y, ψ]], λ[ψ => G[Y, ψ]], Z]
  type Sum[F[_, _], G[_, _], Y, Z] = Xor[F[Y, Z], G[Y, Z]]

  /**
    *
    * special case (sometimes `Y` is not important at all)
    *
    */

  type ConstantUnit[Y, Z] = Constant[Unit, Z]

}

/**
  *
  * Polynomial Functors can, recursively, be defined in terms of
  *
  *  `ConstantFunctor`
  *  `IdentityFunctor`
  *  `ProductFunctor`
  *  `SumFunctor`
  *
  */

class ConstantFunctor[Y] extends Functor[λ[ζ => Y]] {
  def map[Z, X](y: Y)(z2x: Z => X): Y = y
}

class IdentityFunctor[Y] extends Functor[λ[ζ => ζ]] {
  def map[Z, X](z: Z)(z2x: Z => X): X = z2x(z)
}

class ProductFunctor[F[_, _], G[_, _], Y](F: Functor[λ[ζ => F[Y, ζ]]], G: Functor[λ[ζ => G[Y, ζ]]]) extends Functor[λ[ζ => Product[F, G, Y, ζ]]] {
  def map[Z, X](product: Product[F, G, Y, Z])(z2x: Z => X): Product[F, G, Y, X] =
    product.bimap(F.map(_)(z2x), G.map(_)(z2x))
}


class SumFunctor[F[_, _], G[_, _], Y](F: Functor[λ[ζ => F[Y, ζ]]], G: Functor[λ[ζ => G[Y, ζ]]]) extends Functor[λ[ζ => Sum[F, G, Y, ζ]]] {
  def map[Z, X](sum: Sum[F, G, Y, Z])(z2x: Z => X): Sum[F, G, Y, X] =
    sum.bimap(F.map(_)(z2x), G.map(_)(z2x))
}

