package cats

import simulacrum.{noop, typeclass}
import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of Selective for ${F}")
@typeclass trait Selective[F[_]] extends Applicative[F] {
  def select[A, B](fab: F[Either[A, B]])(ff: => F[A => B]): F[B]

  def branch[A, B, C](fab: F[Either[A, B]])(fl: F[A => C])(fr: F[B => C]): F[C] = {
    val innerLhs: F[Either[A, Either[B, C]]] = map(fab)(_.map(Left(_)))
    val innerRhs: F[A => Either[B, C]] = map(fl)(_.andThen(Right(_)))
    val lhs = select(innerLhs)(innerRhs)
    select(lhs)(fr)
  }

  def apS[A, B](ff: F[A => B])(fa: F[A]): F[B] = {
    val left: F[Either[A => B, B]] = map(ff)(Left(_))
    val right: F[(A => B) => B] = map(fa)((a: A) => _(a))
    select(left)(right)
  }

  @noop
  def ifS[A](x: F[Boolean])(t: F[A])(e: F[A]): F[A] = {
    val condition: F[Either[Unit, Unit]] = map(x)(p => if (p) EitherUtil.leftUnit else EitherUtil.unit)
    val left: F[Unit => A] = map(t)(Function.const)
    val right: F[Unit => A] = map(e)(Function.const)
    branch(condition)(left)(right)
  }

  @noop
  def whenS[A](fbool: F[Boolean])(fa: F[Unit]): F[Unit] =
    ifS(fbool)(fa)(unit)
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
    def select[B, C](ff: => F[B => C])(implicit ev$1: A <:< Either[B, C]): F[C] =
      typeClassInstance.select[B, C](self.asInstanceOf[F[Either[B, C]]])(ff)
    def branch[B, C, D](fl: F[B => D])(fr: F[C => D])(implicit ev$1: A <:< Either[B, C]): F[D] =
      typeClassInstance.branch[B, C, D](self.asInstanceOf[F[Either[B, C]]])(fl)(fr)
    def apS[B, C](fa: F[B])(implicit ev$1: A <:< (B => C)): F[C] =
      typeClassInstance.apS[B, C](self.asInstanceOf[F[B => C]])(fa)
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