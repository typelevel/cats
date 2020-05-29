package cats

import cats.kernel.CommutativeMonoid
import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * Commutative Applicative.
 *
 * Further than an Applicative, which just allows composition of independent effectful functions,
 * in a Commutative Applicative those functions can be composed in any order, which guarantees
 * that their effects do not interfere.
 *
 * Must obey the laws defined in cats.laws.CommutativeApplicativeLaws.
 */
@implicitNotFound("Could not find an instance of CommutativeApplicative for ${F}")
@typeclass trait CommutativeApplicative[F[_]] extends Applicative[F] with CommutativeApply[F]

object CommutativeApplicative {
  def commutativeMonoidFor[F[_]: CommutativeApplicative, A: CommutativeMonoid]: CommutativeMonoid[F[A]] =
    new CommutativeMonoid[F[A]] {
      override def empty: F[A] =
        CommutativeApplicative[F]
          .pure(CommutativeMonoid[A].empty)

      override def combine(x: F[A], y: F[A]): F[A] =
        CommutativeApplicative[F]
          .map2(x, y)(CommutativeMonoid[A].combine)
    }

  /****************************************************************************/
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /****************************************************************************/

  /**
   * Summon an instance of [[CommutativeApplicative]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: CommutativeApplicative[F]): CommutativeApplicative[F] = instance

  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: CommutativeApplicative[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A] with Applicative.AllOps[F, A] with CommutativeApply.AllOps[F, A] {
    type TypeClassType <: CommutativeApplicative[F]
  }
  trait ToCommutativeApplicativeOps extends Serializable {
    implicit def toCommutativeApplicativeOps[F[_], A](target: F[A])(implicit tc: CommutativeApplicative[F]): Ops[F, A] {
      type TypeClassType = CommutativeApplicative[F]
    } =
      new Ops[F, A] {
        type TypeClassType = CommutativeApplicative[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  object nonInheritedOps extends ToCommutativeApplicativeOps
  object ops {
    implicit def toAllCommutativeApplicativeOps[F[_], A](
      target: F[A]
    )(implicit tc: CommutativeApplicative[F]): AllOps[F, A] {
      type TypeClassType = CommutativeApplicative[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = CommutativeApplicative[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }

  /****************************************************************************/
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /****************************************************************************/

}
