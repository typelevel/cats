package cats

import simulacrum.typeclass

/**
 * An Invariant Additive Monoidal.
 * Adds the ability to create an `empty` value which must serve as the identity for `choice`.
 *
 * Must obey the laws defined in cats.laws.InvariantChoosableLaws.
 */
@typeclass trait InvariantChoosable[F[_]] extends InvariantChoice[F] {
  def zero: F[Nothing]
}

object InvariantChoosable {
  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[InvariantChoosable]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: InvariantChoosable[F]): InvariantChoosable[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllInvariantChoosableOps[F[_], A](target: F[A])(implicit tc: InvariantChoosable[F]): AllOps[F, A] {
      type TypeClassType = InvariantChoosable[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = InvariantChoosable[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: InvariantChoosable[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A] with InvariantChoice.AllOps[F, A] {
    type TypeClassType <: InvariantChoosable[F]
  }
  trait ToInvariantChoosableOps extends Serializable {
    implicit def toInvariantChoosableOps[F[_], A](target: F[A])(implicit tc: InvariantChoosable[F]): Ops[F, A] {
      type TypeClassType = InvariantChoosable[F]
    } =
      new Ops[F, A] {
        type TypeClassType = InvariantChoosable[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToInvariantChoosableOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
