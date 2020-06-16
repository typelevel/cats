package cats

import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * Comonad
 *
 * Comonad is the dual of Monad. Whereas Monads allow for the composition of effectful functions,
 * Comonads allow for composition of functions that extract the value from their context.
 *
 * Must obey the laws defined in cats.laws.ComonadLaws.
 */
@implicitNotFound("Could not find an instance of Comonad for ${F}")
@typeclass trait Comonad[F[_]] extends CoflatMap[F] {

  /**
   * `extract` is the dual of `pure` on Monad (via `Applicative`)
   * and extracts the value from its context
   *
   * Example:
   * {{{
   * scala> import cats.Id
   * scala> import cats.Comonad
   * scala> val id: Id[Int] = 3
   * scala> Comonad[Id].extract(id)
   * res0: cats.Id[Int] = 3
   * }}}
   */
  def extract[A](x: F[A]): A
}

object Comonad {

  /****************************************************************************/
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /****************************************************************************/

  /**
   * Summon an instance of [[Comonad]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Comonad[F]): Comonad[F] = instance

  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: Comonad[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def extract: A = typeClassInstance.extract[A](self)
  }
  trait AllOps[F[_], A] extends Ops[F, A] with CoflatMap.AllOps[F, A] {
    type TypeClassType <: Comonad[F]
  }
  trait ToComonadOps extends Serializable {
    implicit def toComonadOps[F[_], A](target: F[A])(implicit tc: Comonad[F]): Ops[F, A] {
      type TypeClassType = Comonad[F]
    } =
      new Ops[F, A] {
        type TypeClassType = Comonad[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToComonadOps
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllComonadOps[F[_], A](target: F[A])(implicit tc: Comonad[F]): AllOps[F, A] {
      type TypeClassType = Comonad[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = Comonad[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }

  /****************************************************************************/
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /****************************************************************************/

}
