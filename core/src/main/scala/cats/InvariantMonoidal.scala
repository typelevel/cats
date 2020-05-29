package cats

import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * Invariant version of a Monoidal.
 *
 * Must obey the laws defined in cats.laws.InvariantMonoidalLaws.
 */
@implicitNotFound("Could not find an instance of InvariantMonoidal for ${F}")
@typeclass trait InvariantMonoidal[F[_]] extends InvariantSemigroupal[F] {

  /**
   * `point` lifts any value into a Monoidal Functor.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> InvariantMonoidal[Option].point(10)
   * res0: Option[Int] = Some(10)
   * }}}
   */
  def point[A](a: A): F[A] = imap(unit)(_ => a)(_ => ())

  def unit: F[Unit]

}

object InvariantMonoidal {

  /**
   * Gives a `Monoid` instance if A itself has a `Monoid` instance.
   */
  def monoid[F[_], A](implicit F: InvariantMonoidal[F], A: Monoid[A]): Monoid[F[A]] =
    new InvariantMonoidalMonoid[F, A](F, A)

  /****************************************************************************/
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /****************************************************************************/

  /**
   * Summon an instance of [[InvariantMonoidal]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: InvariantMonoidal[F]): InvariantMonoidal[F] = instance

  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: InvariantMonoidal[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A] with InvariantSemigroupal.AllOps[F, A] {
    type TypeClassType <: InvariantMonoidal[F]
  }
  trait ToInvariantMonoidalOps extends Serializable {
    implicit def toInvariantMonoidalOps[F[_], A](target: F[A])(implicit tc: InvariantMonoidal[F]): Ops[F, A] {
      type TypeClassType = InvariantMonoidal[F]
    } =
      new Ops[F, A] {
        type TypeClassType = InvariantMonoidal[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  object nonInheritedOps extends ToInvariantMonoidalOps
  object ops {
    implicit def toAllInvariantMonoidalOps[F[_], A](target: F[A])(implicit tc: InvariantMonoidal[F]): AllOps[F, A] {
      type TypeClassType = InvariantMonoidal[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = InvariantMonoidal[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }

  /****************************************************************************/
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /****************************************************************************/

}

private[cats] class InvariantMonoidalMonoid[F[_], A](f: InvariantMonoidal[F], monoid: Monoid[A])
    extends InvariantSemigroupalSemigroup(f, monoid)
    with Monoid[F[A]] {
  def empty: F[A] = f.point(monoid.empty)
}
