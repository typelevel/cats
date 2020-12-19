package cats

import simulacrum.{noop, typeclass}
import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of Selective for ${F}")
@typeclass trait Selective[F[_]] extends Applicative[F] {
  @noop
  def whenS[A](fCond: F[Boolean])(fTrue: => F[Unit]): F[Unit] =
    ifS(fCond)(fTrue)(unit)
}

object Selective {
  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[Selective]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Selective[F]): Selective[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllSelectiveOps[F[_], A](target: F[A])(implicit tc: Selective[F]): AllOps[F, A] {
      type TypeClassType = Selective[F]
    } = new AllOps[F, A] {
      type TypeClassType = Selective[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: Selective[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A] with Applicative.AllOps[F, A] {
    type TypeClassType <: Selective[F]
  }
  trait ToSelectiveOps extends Serializable {
    implicit def toSelectiveOps[F[_], A](target: F[A])(implicit tc: Selective[F]): Ops[F, A] {
      type TypeClassType = Selective[F]
    } = new Ops[F, A] {
      type TypeClassType = Selective[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToSelectiveOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
