package cats
package arrow

import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * In a Commutative Arrow F[_, _], the split operation (or `***`) is commutative,
 * which means that there is non-interference between the effect of the paired arrows.
 *
 * Must obey the laws in CommutativeArrowLaws
 */
@implicitNotFound("Could not find an instance of CommutativeArrow for ${F}")
@typeclass trait CommutativeArrow[F[_, _]] extends Arrow[F]

object CommutativeArrow {

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[CommutativeArrow]] for `F`.
   */
  @inline def apply[F[_, _]](implicit instance: CommutativeArrow[F]): CommutativeArrow[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllCommutativeArrowOps[F[_, _], A, B](
      target: F[A, B]
    )(implicit tc: CommutativeArrow[F]): AllOps[F, A, B] {
      type TypeClassType = CommutativeArrow[F]
    } =
      new AllOps[F, A, B] {
        type TypeClassType = CommutativeArrow[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_, _], A, B] extends Serializable {
    type TypeClassType <: CommutativeArrow[F]
    def self: F[A, B]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_, _], A, B] extends Ops[F, A, B] with Arrow.AllOps[F, A, B] {
    type TypeClassType <: CommutativeArrow[F]
  }
  trait ToCommutativeArrowOps extends Serializable {
    implicit def toCommutativeArrowOps[F[_, _], A, B](target: F[A, B])(implicit tc: CommutativeArrow[F]): Ops[F, A, B] {
      type TypeClassType = CommutativeArrow[F]
    } =
      new Ops[F, A, B] {
        type TypeClassType = CommutativeArrow[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToCommutativeArrowOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
