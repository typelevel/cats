package cats

import functor.Contravariant
import simulacrum.typeclass

/**
 * [[Cartesian]] captures the idea of composing independent effectful values.
 * It is of particular interest when taken together with [[Functor]] - where [[Functor]]
 * captures the idea of applying a unary pure function to an effectful value,
 * calling `product` with `map` allows one to apply a function of arbitrary arity to multiple
 * independent effectful values.
 *
 * That same idea is also manifested in the form of [[Apply]], and indeed [[Apply]] extends both
 * [[Cartesian]] and [[Functor]] to illustrate this.
 */
@typeclass trait Cartesian[F[_]] {
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}

object Cartesian extends CartesianArityFunctions with KernelCartesianInstances

/**
 * Cartesian instances for types that are housed in Kernel and therefore
 * can't have instances for Cats type classes in their companion objects.
 */
private[cats] sealed trait KernelCartesianInstances {
  implicit val catsInvariantSemigroup: Cartesian[Semigroup] = InvariantMonoidal.catsInvariantMonoidalSemigroup
  implicit val catsInvariantMonoid: Cartesian[Monoid] = InvariantMonoidal.catsInvariantMonoidalMonoid
  implicit val catsCartesianEq: Cartesian[Eq] = new Cartesian[Eq] {
    def product[A, B](fa: Eq[A], fb: Eq[B]): Eq[(A, B)] =
      Eq.instance { (left, right) => fa.eqv(left._1, right._1) && fb.eqv(left._2, right._2) }
  }

  implicit def catsCartesianComposeContravariantFunctor[F[_], G[_]](
    implicit C: Cartesian[F], F: Contravariant[F], G: Functor[G]): Cartesian[λ[α => F[G[α]]]] =
      new Cartesian[λ[α => F[G[α]]]] {
        def product[A, B](fa: F[G[A]], fb: F[G[B]]): F[G[(A, B)]] =
          F.contramap(C.product(fa, fb)) { g: G[(A, B)] =>
            (G.map(g)(_._1), G.map(g)(_._2))
          }
      }
}
