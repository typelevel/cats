package cats
package arrow

import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * A [[Profunctor]] is a [[Contravariant]] functor on its first type parameter
 * and a [[Functor]] on its second type parameter.
 *
 * Must obey the laws defined in cats.laws.ProfunctorLaws.
 */
@implicitNotFound("Could not find an instance of Profunctor for ${F}")
@typeclass trait Profunctor[F[_, _]] extends Serializable { self =>

  /**
   * Contramap on the first type parameter and map on the second type parameter
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.arrow.Profunctor
   * scala> val fab: Double => Double = x => x + 0.3
   * scala> val f: Int => Double = x => x.toDouble / 2
   * scala> val g: Double => Double = x => x * 3
   * scala> val h = Profunctor[Function1].dimap(fab)(f)(g)
   * scala> h(3)
   * res0: Double = 5.4
   * }}}
   */
  def dimap[A, B, C, D](fab: F[A, B])(f: C => A)(g: B => D): F[C, D]

  /**
   * contramap on the first type parameter
   */
  def lmap[A, B, C](fab: F[A, B])(f: C => A): F[C, B] =
    dimap(fab)(f)(identity)

  /**
   * map on the second type parameter
   */
  def rmap[A, B, C](fab: F[A, B])(f: B => C): F[A, C] =
    dimap[A, B, A, C](fab)(identity)(f)
}

object Profunctor {
  implicit def catsStrongForFunction1: Strong[Function1] =
    cats.instances.function.catsStdInstancesForFunction1

  implicit def catsStrongForPartialFunction: Strong[PartialFunction] =
    cats.instances.partialFunction.catsStdInstancesForPartialFunction

  /****************************************************************************/
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /****************************************************************************/

  /**
   * Summon an instance of [[Profunctor]] for `F`.
   */
  @inline def apply[F[_, _]](implicit instance: Profunctor[F]): Profunctor[F] = instance

  trait Ops[F[_, _], A, B] extends Serializable {
    type TypeClassType <: Profunctor[F]
    def self: F[A, B]
    val typeClassInstance: TypeClassType
    def dimap[C, D](f: C => A)(g: B => D): F[C, D] = typeClassInstance.dimap[A, B, C, D](self)(f)(g)
    def lmap[C](f: C => A): F[C, B] = typeClassInstance.lmap[A, B, C](self)(f)
    def rmap[C](f: B => C): F[A, C] = typeClassInstance.rmap[A, B, C](self)(f)
  }
  trait AllOps[F[_, _], A, B] extends Ops[F, A, B]
  trait ToProfunctorOps extends Serializable {
    implicit def toProfunctorOps[F[_, _], A, B](target: F[A, B])(implicit tc: Profunctor[F]): Ops[F, A, B] {
      type TypeClassType = Profunctor[F]
    } =
      new Ops[F, A, B] {
        type TypeClassType = Profunctor[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToProfunctorOps
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllProfunctorOps[F[_, _], A, B](target: F[A, B])(implicit tc: Profunctor[F]): AllOps[F, A, B] {
      type TypeClassType = Profunctor[F]
    } =
      new AllOps[F, A, B] {
        type TypeClassType = Profunctor[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }

  /****************************************************************************/
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /****************************************************************************/

}
