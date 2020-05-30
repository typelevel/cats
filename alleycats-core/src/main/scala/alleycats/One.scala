package alleycats

import cats.Eq
import cats.syntax.eq._
import simulacrum.typeclass
import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of One for ${A}")
@typeclass trait One[A] extends Serializable {
  def one: A

  def isOne(a: A)(implicit ev: Eq[A]): Boolean =
    one === a

  def nonOne(a: A)(implicit ev: Eq[A]): Boolean =
    one =!= a
}

object One {
  def apply[A](a: => A): One[A] =
    new One[A] { lazy val one: A = a }

  /****************************************************************************/
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /****************************************************************************/
  /**
   * Summon an instance of [[One]] for `A`.
   */
  @inline def apply[A](implicit instance: One[A]): One[A] = instance

  trait Ops[A] {
    type TypeClassType <: One[A]
    def self: A
    val typeClassInstance: TypeClassType
    def isOne(implicit ev: Eq[A]): Boolean = typeClassInstance.isOne(self)(ev)
    def nonOne(implicit ev: Eq[A]): Boolean = typeClassInstance.nonOne(self)(ev)
  }
  trait AllOps[A] extends Ops[A]
  trait ToOneOps {
    implicit def toOneOps[A](target: A)(implicit tc: One[A]): Ops[A] {
      type TypeClassType = One[A]
    } =
      new Ops[A] {
        type TypeClassType = One[A]
        val self: A = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  object nonInheritedOps extends ToOneOps
  object ops {
    implicit def toAllOneOps[A](target: A)(implicit tc: One[A]): AllOps[A] {
      type TypeClassType = One[A]
    } =
      new AllOps[A] {
        type TypeClassType = One[A]
        val self: A = target
        val typeClassInstance: TypeClassType = tc
      }
  }

  /****************************************************************************/
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /****************************************************************************/

}
