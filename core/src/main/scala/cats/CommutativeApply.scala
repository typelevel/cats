package cats

import simulacrum.typeclass

/**
  * Commutative Apply.
  *
  * Further than an Apply, which just allows composition of independent effectful functions,
  * in a Commutative Apply those functions can be composed in any order, which guarantees
  * that their effects do not interfere.
  *
  * Must obey the laws defined in cats.laws.CommutativeApplyLaws.
  */
@typeclass trait CommutativeApply[F[_]] extends Apply[F]
