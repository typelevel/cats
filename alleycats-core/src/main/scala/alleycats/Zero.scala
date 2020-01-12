package alleycats

import cats.Eq
import cats.syntax.eq._

import simulacrum.typeclass
import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of Zero for ${A}")
@typeclass trait Zero[A] extends Serializable {
  def zero: A

  def isZero(a: A)(implicit ev: Eq[A]): Boolean =
    zero === a

  def nonZero(a: A)(implicit ev: Eq[A]): Boolean =
    zero =!= a
}

object Zero {
  def apply[A](a: => A): Zero[A] =
    new Zero[A] { lazy val zero: A = a }

  /****************************************************************************
   * THE REST OF THIS OBJECT IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!! *
   ****************************************************************************/
  /**
   * Summon an instance of [[Zero]] for `A`.
   */
  @inline def apply[A](implicit instance: Zero[A]): Zero[A] = instance

  trait Ops[A] {
    type TypeClassType <: Zero[A]
    def self: A
    val typeClassInstance: TypeClassType
    def isZero(implicit ev: Eq[A]): Boolean = typeClassInstance.isZero(self)(ev)
    def nonZero(implicit ev: Eq[A]): Boolean = typeClassInstance.nonZero(self)(ev)
  }
  trait AllOps[A] extends Ops[A]
  trait ToZeroOps {
    implicit def toZeroOps[A](target: A)(implicit tc: Zero[A]): Ops[A] {
      type TypeClassType = Zero[A]
    } = new Ops[A] {
      type TypeClassType = Zero[A]
      val self: A = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  object nonInheritedOps extends ToZeroOps
  object ops {
    implicit def toAllZeroOps[A](target: A)(implicit tc: Zero[A]): AllOps[A] {
      type TypeClassType = Zero[A]
    } = new AllOps[A] {
      type TypeClassType = Zero[A]
      val self: A = target
      val typeClassInstance: TypeClassType = tc
    }
  }
}
