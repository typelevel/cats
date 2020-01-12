package cats

import cats.kernel.CommutativeSemigroup
import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * Commutative Apply.
 *
 * Further than an Apply, which just allows composition of independent effectful functions,
 * in a Commutative Apply those functions can be composed in any order, which guarantees
 * that their effects do not interfere.
 *
 * Must obey the laws defined in cats.laws.CommutativeApplyLaws.
 */
@implicitNotFound("Could not find an instance of CommutativeApply for ${F}")
@typeclass trait CommutativeApply[F[_]] extends Apply[F]

object CommutativeApply {
  def commutativeSemigroupFor[F[_]: CommutativeApply, A: CommutativeSemigroup]: CommutativeSemigroup[F[A]] =
    new CommutativeSemigroup[F[A]] {
      override def combine(x: F[A], y: F[A]): F[A] =
        CommutativeApply[F]
          .map2(x, y)(CommutativeSemigroup[A].combine)
    }

  /****************************************************************************
   * THE REST OF THIS OBJECT IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!! *
   ****************************************************************************/
  /**
   * Summon an instance of [[CommutativeApply]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: CommutativeApply[F]): CommutativeApply[F] = instance

  trait Ops[F[_], A] {
    type TypeClassType <: CommutativeApply[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A] with Apply.AllOps[F, A] {
    type TypeClassType <: CommutativeApply[F]
  }
  trait ToCommutativeApplyOps {
    implicit def toCommutativeApplyOps[F[_], A](target: F[A])(implicit tc: CommutativeApply[F]): Ops[F, A] {
      type TypeClassType = CommutativeApply[F]
    } = new Ops[F, A] {
      type TypeClassType = CommutativeApply[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  object nonInheritedOps extends ToCommutativeApplyOps
  object ops {
    implicit def toAllCommutativeApplyOps[F[_], A](target: F[A])(implicit tc: CommutativeApply[F]): AllOps[F, A] {
      type TypeClassType = CommutativeApply[F]
    } = new AllOps[F, A] {
      type TypeClassType = CommutativeApply[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
}
