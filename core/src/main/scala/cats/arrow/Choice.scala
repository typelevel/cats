package cats
package arrow

import simulacrum.typeclass
import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of Choice for ${F}")
@typeclass trait Choice[F[_, _]] extends Category[F] {

  /**
   * Given two `F`s (`f` and `g`) with a common target type, create a new `F`
   * with the same target type, but with a source type of either `f`'s source
   * type OR `g`'s source type.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val b: Boolean => String = _.toString + " is a boolean"
   * scala> val i: Int => String =  _.toString + " is an integer"
   * scala> val f: (Either[Boolean, Int]) => String = b ||| i
   *
   * scala> f(Right(3))
   * res0: String = 3 is an integer
   *
   * scala> f(Left(false))
   * res0: String = false is a boolean
   * }}}
   */
  @simulacrum.op("|||", alias = true)
  def choice[A, B, C](f: F[A, C], g: F[B, C]): F[Either[A, B], C]

  /**
   * An `F` that, given a source `A` on either the right or left side, will
   * return that same `A` object.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val f: (Either[Int, Int]) => Int = Choice[Function1].codiagonal[Int]
   *
   * scala> f(Right(3))
   * res0: Int = 3
   *
   * scala> f(Left(3))
   * res1: Int = 3
   * }}}
   */
  def codiagonal[A]: F[Either[A, A], A] = choice(id, id)
}

object Choice {

  /****************************************************************************/
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /****************************************************************************/

  /**
   * Summon an instance of [[Choice]] for `F`.
   */
  @inline def apply[F[_, _]](implicit instance: Choice[F]): Choice[F] = instance

  trait Ops[F[_, _], A, B] extends Serializable {
    type TypeClassType <: Choice[F]
    def self: F[A, B]
    val typeClassInstance: TypeClassType
    def choice[C](g: F[C, B]): F[Either[A, C], B] = typeClassInstance.choice[A, C, B](self, g)
    def |||[C](g: F[C, B]): F[Either[A, C], B] = typeClassInstance.choice[A, C, B](self, g)
  }
  trait AllOps[F[_, _], A, B] extends Ops[F, A, B] with Category.AllOps[F, A, B] {
    type TypeClassType <: Choice[F]
  }
  trait ToChoiceOps extends Serializable {
    implicit def toChoiceOps[F[_, _], A, B](target: F[A, B])(implicit tc: Choice[F]): Ops[F, A, B] {
      type TypeClassType = Choice[F]
    } =
      new Ops[F, A, B] {
        type TypeClassType = Choice[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToChoiceOps
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllChoiceOps[F[_, _], A, B](target: F[A, B])(implicit tc: Choice[F]): AllOps[F, A, B] {
      type TypeClassType = Choice[F]
    } =
      new AllOps[F, A, B] {
        type TypeClassType = Choice[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }

  /****************************************************************************/
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /****************************************************************************/

}
