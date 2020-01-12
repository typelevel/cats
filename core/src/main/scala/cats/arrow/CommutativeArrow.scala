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

  /****************************************************************************
   * THE REST OF THIS OBJECT IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!! *
   ****************************************************************************/
  /**
   * Summon an instance of [[CommutativeArrow]] for `F`.
   */
  @inline def apply[F[_, _]](implicit instance: CommutativeArrow[F]): CommutativeArrow[F] = instance

  trait Ops[F[_, _], A, B] {
    type TypeClassType <: CommutativeArrow[F]
    def self: F[A, B]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_, _], A, B] extends Ops[F, A, B] with Arrow.AllOps[F, A, B] {
    type TypeClassType <: CommutativeArrow[F]
  }
  trait ToCommutativeArrowOps {
    implicit def toCommutativeArrowOps[F[_, _], A, B](target: F[A, B])(implicit tc: CommutativeArrow[F]): Ops[F, A, B] {
      type TypeClassType = CommutativeArrow[F]
    } = new Ops[F, A, B] {
      type TypeClassType = CommutativeArrow[F]
      val self: F[A, B] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  object nonInheritedOps extends ToCommutativeArrowOps
  object ops {
    implicit def toAllCommutativeArrowOps[F[_, _], A, B](
      target: F[A, B]
    )(implicit tc: CommutativeArrow[F]): AllOps[F, A, B] {
      type TypeClassType = CommutativeArrow[F]
    } = new AllOps[F, A, B] {
      type TypeClassType = CommutativeArrow[F]
      val self: F[A, B] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
}
