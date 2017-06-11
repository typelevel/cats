package cats

import simulacrum.typeclass

/**
 * Commutative CoflatMap.
 *
 * Further than a CoflatMap, which just allows composition of dependent effectful functions,
 * in a Commutative CoflatMap those functions can be composed in any order, which guarantees
 * that their effects do not interfere.
 *
 * Must obey the laws defined in cats.laws.CommutativeCoflatMapLaws.
 */
@typeclass trait CommutativeCoflatMap[F[_]] extends CoflatMap[F]
