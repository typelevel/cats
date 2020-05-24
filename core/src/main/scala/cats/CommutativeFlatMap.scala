package cats

import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * Commutative FlatMap.
 *
 * Further than a FlatMap, which just allows composition of dependent effectful functions,
 * in a Commutative FlatMap those functions can be composed in any order, which guarantees
 * that their effects do not interfere.
 *
 * Must obey the laws defined in cats.laws.CommutativeFlatMapLaws.
 */
@implicitNotFound("Could not find an instance of CommutativeFlatMap for ${F}")
@typeclass trait CommutativeFlatMap[F[_]] extends FlatMap[F] with CommutativeApply[F]

object CommutativeFlatMap {

  /****************************************************************************/
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /****************************************************************************/
  /**
   * Summon an instance of [[CommutativeFlatMap]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: CommutativeFlatMap[F]): CommutativeFlatMap[F] = instance

  trait Ops[F[_], A] {
    type TypeClassType <: CommutativeFlatMap[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A] with FlatMap.AllOps[F, A] with CommutativeApply.AllOps[F, A] {
    type TypeClassType <: CommutativeFlatMap[F]
  }
  trait ToCommutativeFlatMapOps {
    implicit def toCommutativeFlatMapOps[F[_], A](target: F[A])(implicit tc: CommutativeFlatMap[F]): Ops[F, A] {
      type TypeClassType = CommutativeFlatMap[F]
    } = new Ops[F, A] {
      type TypeClassType = CommutativeFlatMap[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  object nonInheritedOps extends ToCommutativeFlatMapOps
  object ops {
    implicit def toAllCommutativeFlatMapOps[F[_], A](target: F[A])(implicit tc: CommutativeFlatMap[F]): AllOps[F, A] {
      type TypeClassType = CommutativeFlatMap[F]
    } = new AllOps[F, A] {
      type TypeClassType = CommutativeFlatMap[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }

  /****************************************************************************/
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /****************************************************************************/

}
