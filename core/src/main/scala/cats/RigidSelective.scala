package cats

import simulacrum.typeclass
import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of RigidSelective for ${F}")
@typeclass trait RigidSelective[F[_]] extends Selective[F]

object RigidSelective {
  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[RigidSelective]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: RigidSelective[F]): RigidSelective[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllRigidSelectiveOps[F[_], A](target: F[A])(implicit tc: RigidSelective[F]): AllOps[F, A] {
      type TypeClassType = RigidSelective[F]
    } = new AllOps[F, A] {
      type TypeClassType = RigidSelective[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: RigidSelective[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A] with Selective.AllOps[F, A] {
    type TypeClassType <: RigidSelective[F]
  }
  trait ToRigidSelectiveOps extends Serializable {
    implicit def toRigidSelectiveOps[F[_], A](target: F[A])(implicit tc: RigidSelective[F]): Ops[F, A] {
      type TypeClassType = RigidSelective[F]
    } = new Ops[F, A] {
      type TypeClassType = RigidSelective[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToRigidSelectiveOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
