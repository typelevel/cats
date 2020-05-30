package cats

import simulacrum.typeclass
import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of Bimonad for ${F}")
@typeclass trait Bimonad[F[_]] extends Monad[F] with Comonad[F]

object Bimonad {

  /****************************************************************************/
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /****************************************************************************/
  /**
   * Summon an instance of [[Bimonad]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Bimonad[F]): Bimonad[F] = instance

  trait Ops[F[_], A] {
    type TypeClassType <: Bimonad[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A] with Monad.AllOps[F, A] with Comonad.AllOps[F, A] {
    type TypeClassType <: Bimonad[F]
  }
  trait ToBimonadOps {
    implicit def toBimonadOps[F[_], A](target: F[A])(implicit tc: Bimonad[F]): Ops[F, A] {
      type TypeClassType = Bimonad[F]
    } =
      new Ops[F, A] {
        type TypeClassType = Bimonad[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  object nonInheritedOps extends ToBimonadOps
  object ops {
    implicit def toAllBimonadOps[F[_], A](target: F[A])(implicit tc: Bimonad[F]): AllOps[F, A] {
      type TypeClassType = Bimonad[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = Bimonad[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }

  /****************************************************************************/
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /****************************************************************************/

}
