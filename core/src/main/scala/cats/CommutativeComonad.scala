package cats

import simulacrum.typeclass

/**
 * Commutative Comonad.
 *
 * Further than a Comonad, which just allows composition of dependent effectful functions,
 * in a Commutative Comonad those functions can be composed in any order, which guarantees
 * that their effects do not interfere.
 *
 * Must obey the laws defined in cats.laws.CommutativeComonadLaws.
 */
@typeclass trait CommutativeComonad[F[_]] extends Comonad[F] with CommutativeCoflatMap[F]
