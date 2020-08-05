package cats

import simulacrum.typeclass

/**
 * An [[InvariantSemiringal]] is both an Additive and Multiplicative Invariant Monoidal.
 * `zero` must absorb `product`.
 *
 * Must obey the laws defined in cats.laws.InvariantSemiringalLaws.
 */
@typeclass trait InvariantSemiringal[F[_]] extends InvariantMonoidal[F] with InvariantChoosable[F] {
  override def invariant: Invariant[F] = this
}

object InvariantSemiringal {

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[InvariantSemiringal]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: InvariantSemiringal[F]): InvariantSemiringal[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllInvariantSemiringalOps[F[_], A](target: F[A])(implicit tc: InvariantSemiringal[F]): AllOps[F, A] {
      type TypeClassType = InvariantSemiringal[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = InvariantSemiringal[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: InvariantSemiringal[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A] with InvariantMonoidal.AllOps[F, A] with InvariantChoosable.AllOps[F, A] {
    type TypeClassType <: InvariantSemiringal[F]
  }
  trait ToInvariantSemiringalOps extends Serializable {
    implicit def toInvariantSemiringalOps[F[_], A](target: F[A])(implicit tc: InvariantSemiringal[F]): Ops[F, A] {
      type TypeClassType = InvariantSemiringal[F]
    } =
      new Ops[F, A] {
        type TypeClassType = InvariantSemiringal[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToInvariantSemiringalOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
