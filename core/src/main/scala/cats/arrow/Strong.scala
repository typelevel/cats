package cats
package arrow

import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * Must obey the laws defined in cats.laws.StrongLaws.
 */
@implicitNotFound("Could not find an instance of Strong for ${F}")
@typeclass trait Strong[F[_, _]] extends Profunctor[F] {

  /**
   * Create a new `F` that takes two inputs, but only modifies the first input
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.arrow.Strong
   * scala> val f: Int => Int = _ * 2
   * scala> val fab = Strong[Function1].first[Int,Int,Int](f)
   * scala> fab((2,3))
   * res0: (Int, Int) = (4,3)
   * }}}
   */
  def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)]

  /**
   * Create a new `F` that takes two inputs, but only modifies the second input
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.arrow.Strong
   * scala> val f: Int => Int = _ * 2
   * scala> val fab = Strong[Function1].second[Int,Int,Int](f)
   * scala> fab((2,3))
   * res0: (Int, Int) = (2,6)
   * }}}
   */
  def second[A, B, C](fa: F[A, B]): F[(C, A), (C, B)]
}

object Strong {

  /****************************************************************************/
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /****************************************************************************/

  /**
   * Summon an instance of [[Strong]] for `F`.
   */
  @inline def apply[F[_, _]](implicit instance: Strong[F]): Strong[F] = instance

  trait Ops[F[_, _], A, B] extends Serializable {
    type TypeClassType <: Strong[F]
    def self: F[A, B]
    val typeClassInstance: TypeClassType
    def first[C]: F[(A, C), (B, C)] = typeClassInstance.first[A, B, C](self)
    def second[C]: F[(C, A), (C, B)] = typeClassInstance.second[A, B, C](self)
  }
  trait AllOps[F[_, _], A, B] extends Ops[F, A, B] with Profunctor.AllOps[F, A, B] {
    type TypeClassType <: Strong[F]
  }
  trait ToStrongOps extends Serializable {
    implicit def toStrongOps[F[_, _], A, B](target: F[A, B])(implicit tc: Strong[F]): Ops[F, A, B] {
      type TypeClassType = Strong[F]
    } =
      new Ops[F, A, B] {
        type TypeClassType = Strong[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToStrongOps
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllStrongOps[F[_, _], A, B](target: F[A, B])(implicit tc: Strong[F]): AllOps[F, A, B] {
      type TypeClassType = Strong[F]
    } =
      new AllOps[F, A, B] {
        type TypeClassType = Strong[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }

  /****************************************************************************/
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /****************************************************************************/

}
