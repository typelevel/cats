package cats

import simulacrum.typeclass

/**
 * An [[InvariantSemiringal]] is both an Additive and Multiplicative Invariant Monoidal.
 *
 * Must obey the laws defined in cats.laws.InvariantSemiringalLaws.
 */
@typeclass trait InvariantSemiringal[F[_]] extends InvariantMonoidal[F] with InvariantAddMonoidal[F] {
  override def invariant: Invariant[F] = this
}
