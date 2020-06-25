package alleycats

import cats.{Eq, Monoid}
import cats.syntax.eq._

import simulacrum.typeclass
import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of Empty for ${A}")
@typeclass trait Empty[A] extends Serializable {
  def empty: A

  def isEmpty(a: A)(implicit ev: Eq[A]): Boolean =
    empty === a

  def nonEmpty(a: A)(implicit ev: Eq[A]): Boolean =
    empty =!= a
}

object Empty extends EmptyInstances0 {
  def apply[A](a: => A): Empty[A] =
    new Empty[A] { lazy val empty: A = a }

  def fromEmptyK[F[_], T](implicit ekf: EmptyK[F]): Empty[F[T]] = ekf.synthesize[T]

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[Empty]] for `A`.
   */
  @inline def apply[A](implicit instance: Empty[A]): Empty[A] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllEmptyOps[A](target: A)(implicit tc: Empty[A]): AllOps[A] {
      type TypeClassType = Empty[A]
    } =
      new AllOps[A] {
        type TypeClassType = Empty[A]
        val self: A = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[A] extends Serializable {
    type TypeClassType <: Empty[A]
    def self: A
    val typeClassInstance: TypeClassType
    def isEmpty(implicit ev: Eq[A]): Boolean = typeClassInstance.isEmpty(self)(ev)
    def nonEmpty(implicit ev: Eq[A]): Boolean = typeClassInstance.nonEmpty(self)(ev)
  }
  trait AllOps[A] extends Ops[A]
  trait ToEmptyOps extends Serializable {
    implicit def toEmptyOps[A](target: A)(implicit tc: Empty[A]): Ops[A] {
      type TypeClassType = Empty[A]
    } =
      new Ops[A] {
        type TypeClassType = Empty[A]
        val self: A = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToEmptyOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}

private[alleycats] trait EmptyInstances0 extends compat.IterableEmptyInstance with EmptyInstances1

private[alleycats] trait EmptyInstances1 {
  // If Monoid extended Empty then this could be an exported subclass instance provided by Monoid
  implicit def monoidIsEmpty[A: Monoid]: Empty[A] =
    Empty(Monoid[A].empty)
}
