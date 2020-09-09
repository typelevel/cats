package cats
package arrow

import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * Must obey the laws defined in cats.laws.ArrowChoiceLaws.
 */
@implicitNotFound("Could not find an instance of ArrowChoice for ${F}")
@typeclass trait ArrowChoice[F[_, _]] extends Arrow[F] with Choice[F] { self =>

  /**
   * ArrowChoice yields Arrows with choice, allowing distribution
   * over coproducts.
   *
   * Given two `F`s (`f` and `g`), create a new `F` with
   * domain the coproduct of the domains of `f` and `g`,
   * and codomain the coproduct of the codomains of `f` and `g`.
   * This is the sum notion to `split`'s product.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> val toLong: Int => Long = _.toLong
   * scala> val toDouble: Float => Double = _.toDouble
   * scala> val f: Either[Int, Float] => Either[Long, Double] = toLong +++ toDouble
   * scala> f(Left(3))
   * res0: Either[Long,Double] = Left(3)
   * scala> f(Right(3))
   * res1: Either[Long,Double] = Right(3.0)
   * }}}
   */
  @simulacrum.op("+++", alias = true)
  def choose[A, B, C, D](f: F[A, C])(g: F[B, D]): F[Either[A, B], Either[C, D]]

  def left[A, B, C](fab: F[A, B]): F[Either[A, C], Either[B, C]] =
    choose(fab)(lift(identity[C]))

  def right[A, B, C](fab: F[A, B]): F[Either[C, A], Either[C, B]] =
    choose(lift(identity[C]))(fab)

  override def choice[A, B, C](f: F[A, C], g: F[B, C]): F[Either[A, B], C] =
    rmap(choose(f)(g))(_.fold(identity, identity))
}

object ArrowChoice {

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[ArrowChoice]] for `F`.
   */
  @inline def apply[F[_, _]](implicit instance: ArrowChoice[F]): ArrowChoice[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllArrowChoiceOps[F[_, _], A, B](target: F[A, B])(implicit tc: ArrowChoice[F]): AllOps[F, A, B] {
      type TypeClassType = ArrowChoice[F]
    } =
      new AllOps[F, A, B] {
        type TypeClassType = ArrowChoice[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_, _], A, B] extends Serializable {
    type TypeClassType <: ArrowChoice[F]
    def self: F[A, B]
    val typeClassInstance: TypeClassType
    def choose[C, D](g: F[C, D]): F[Either[A, C], Either[B, D]] = typeClassInstance.choose[A, C, B, D](self)(g)
    def +++[C, D](g: F[C, D]): F[Either[A, C], Either[B, D]] = typeClassInstance.choose[A, C, B, D](self)(g)
    def left[C]: F[Either[A, C], Either[B, C]] = typeClassInstance.left[A, B, C](self)
    def right[C]: F[Either[C, A], Either[C, B]] = typeClassInstance.right[A, B, C](self)
  }
  trait AllOps[F[_, _], A, B] extends Ops[F, A, B] with Arrow.AllOps[F, A, B] with Choice.AllOps[F, A, B] {
    type TypeClassType <: ArrowChoice[F]
  }
  trait ToArrowChoiceOps extends Serializable {
    implicit def toArrowChoiceOps[F[_, _], A, B](target: F[A, B])(implicit tc: ArrowChoice[F]): Ops[F, A, B] {
      type TypeClassType = ArrowChoice[F]
    } =
      new Ops[F, A, B] {
        type TypeClassType = ArrowChoice[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToArrowChoiceOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
