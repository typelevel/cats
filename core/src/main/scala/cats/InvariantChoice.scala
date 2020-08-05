package cats

import simulacrum.typeclass

/**
 * [[InvariantChoice]] captures the idea of composing independent effectful values into a sum type.
 * It can be seen as an [[InvariantSemigroupal]] that produces a sum type instead of a product type.
 *
 * Must obey the laws defined in cats.laws.InvariantChoiceLaws.
 */
@typeclass trait InvariantChoice[F[_]] extends Serializable {
  def invariant: Invariant[F]

  /**
   * Combine two effectful values into an Either, must be associative.
   */
  def choice[A, B](fa: F[A], fb: F[B]): F[Either[A, B]]
}

object InvariantChoice {
  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[InvariantChoice]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: InvariantChoice[F]): InvariantChoice[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllInvariantChoiceOps[F[_], A](target: F[A])(implicit tc: InvariantChoice[F]): AllOps[F, A] {
      type TypeClassType = InvariantChoice[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = InvariantChoice[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: InvariantChoice[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def choice[B](fb: F[B]): F[Either[A, B]] = typeClassInstance.choice[A, B](self, fb)
  }
  trait AllOps[F[_], A] extends Ops[F, A]
  trait ToInvariantChoiceOps extends Serializable {
    implicit def toInvariantChoiceOps[F[_], A](target: F[A])(implicit tc: InvariantChoice[F]): Ops[F, A] {
      type TypeClassType = InvariantChoice[F]
    } =
      new Ops[F, A] {
        type TypeClassType = InvariantChoice[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToInvariantChoiceOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
