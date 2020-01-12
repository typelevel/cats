package cats

import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * Commutative Monad.
 *
 * Further than a Monad, which just allows composition of dependent effectful functions,
 * in a Commutative Monad those functions can be composed in any order, which guarantees
 * that their effects do not interfere.
 *
 * Must obey the laws defined in cats.laws.CommutativeMonadLaws.
 */
@implicitNotFound("Could not find an instance of CommutativeMonad for ${F}")
@typeclass trait CommutativeMonad[F[_]] extends Monad[F] with CommutativeFlatMap[F] with CommutativeApplicative[F]

object CommutativeMonad {

  /****************************************************************************
   * THE REST OF THIS OBJECT IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!! *
   ****************************************************************************/
  /**
   * Summon an instance of [[CommutativeMonad]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: CommutativeMonad[F]): CommutativeMonad[F] = instance

  trait Ops[F[_], A] {
    type TypeClassType <: CommutativeMonad[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A]
      extends Ops[F, A]
      with Monad.AllOps[F, A]
      with CommutativeFlatMap.AllOps[F, A]
      with CommutativeApplicative.AllOps[F, A] {
    type TypeClassType <: CommutativeMonad[F]
  }
  trait ToCommutativeMonadOps {
    implicit def toCommutativeMonadOps[F[_], A](target: F[A])(implicit tc: CommutativeMonad[F]): Ops[F, A] {
      type TypeClassType = CommutativeMonad[F]
    } = new Ops[F, A] {
      type TypeClassType = CommutativeMonad[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  object nonInheritedOps extends ToCommutativeMonadOps
  object ops {
    implicit def toAllCommutativeMonadOps[F[_], A](target: F[A])(implicit tc: CommutativeMonad[F]): AllOps[F, A] {
      type TypeClassType = CommutativeMonad[F]
    } = new AllOps[F, A] {
      type TypeClassType = CommutativeMonad[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
}
