package cats

import simulacrum.typeclass

/**
 * Commutative Monad.
 *
 * Further than a Monad, which just allows composition of dependent effectful functions,
 * in a Commutative Monad those functions can be composed in any order, which guarantees
 * that their effects do not interfere.
 *
 * Must obey the laws defined in cats.laws.CommutativeMonadLaws.
 */
@typeclass trait CommutativeMonad[F[_]] extends Monad[F] with CommutativeFlatMap[F] with CommutativeApplicative[F]
