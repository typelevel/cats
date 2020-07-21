package cats

import cats.data.INothing
import simulacrum.typeclass

import scala.annotation.implicitNotFound

/**
 * [[Decidable]] functors are functors that supply
 * a `decide` operation allowing choices to be made.
 *
 * This is comparable to [[Alternative]] in the
 * covariant case.
 *
 * Must obey laws in cats.laws.DecidableLaws.
 *
 * Based on ekmett's contravariant library:
 * https://hackage.haskell.org/package/contravariant-1.4/docs/Data-Functor-Contravariant-Divisible.html#g:2
 */
@implicitNotFound("Could not find an instance of Decidable for ${F}")
@typeclass trait Decidable[F[_]] extends ContravariantMonoidal[F] {
  def sum[A, B](fa: F[A], fb: F[B]): F[Either[A, B]]

  def decide[A, B, C](fa: F[A], fb: F[B])(f: C => Either[A, B]): F[C] =
    contramap(sum(fa, fb))(f)

  def chosen[B, C](fb: F[B], fc: F[C]): F[Either[B, C]] = sum(fb, fc)
  def lose[A](f: A => INothing): F[A] = contramap[INothing, A](zero)(f)
  def zero[A]: F[INothing]
}

object Decidable {
  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[Decidable]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Decidable[F]): Decidable[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllDecidableOps[F[_], A](target: F[A])(implicit tc: Decidable[F]): AllOps[F, A] {
      type TypeClassType = Decidable[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = Decidable[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: Decidable[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def sum[B](fb: F[B]): F[Either[A, B]] = typeClassInstance.sum[A, B](self, fb)
    def decide[B, C](fb: F[B])(f: C => Either[A, B]): F[C] = typeClassInstance.decide[A, B, C](self, fb)(f)
    def chosen[B](fc: F[B]): F[Either[A, B]] = typeClassInstance.chosen[A, B](self, fc)
  }
  trait AllOps[F[_], A] extends Ops[F, A] with ContravariantMonoidal.AllOps[F, A] {
    type TypeClassType <: Decidable[F]
  }
  trait ToDecidableOps extends Serializable {
    implicit def toDecidableOps[F[_], A](target: F[A])(implicit tc: Decidable[F]): Ops[F, A] {
      type TypeClassType = Decidable[F]
    } =
      new Ops[F, A] {
        type TypeClassType = Decidable[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToDecidableOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
