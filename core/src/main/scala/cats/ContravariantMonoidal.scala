package cats

import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * [[ContravariantMonoidal]] functors are functors that supply
 * a unit along the diagonal map for the `contramap2` operation.
 *
 * Must obey the laws defined in cats.laws.ContravariantMonoidalLaws.
 *
 * Based on ekmett's contravariant library:
 * https://hackage.haskell.org/package/contravariant-1.4/docs/Data-Functor-Contravariant-Divisible.html
 */
@implicitNotFound("Could not find an instance of ContravariantMonoidal for ${F}")
@typeclass trait ContravariantMonoidal[F[_]] extends ContravariantSemigroupal[F] with InvariantMonoidal[F] {

  /**
   * `trivial` produces an instance of `F` for any type `A`
   * that is trivial with respect to `contramap2` along
   * the diagonal
   */
  def trivial[A]: F[A] = contramap(unit)(_ => ())

}
object ContravariantMonoidal extends SemigroupalArityFunctions {
  def monoid[F[_], A](implicit f: ContravariantMonoidal[F]): Monoid[F[A]] =
    new ContravariantMonoidalMonoid[F, A](f)

  /****************************************************************************/
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /****************************************************************************/

  /**
   * Summon an instance of [[ContravariantMonoidal]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: ContravariantMonoidal[F]): ContravariantMonoidal[F] = instance

  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: ContravariantMonoidal[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A]
      extends Ops[F, A]
      with ContravariantSemigroupal.AllOps[F, A]
      with InvariantMonoidal.AllOps[F, A] {
    type TypeClassType <: ContravariantMonoidal[F]
  }
  trait ToContravariantMonoidalOps extends Serializable {
    implicit def toContravariantMonoidalOps[F[_], A](target: F[A])(implicit tc: ContravariantMonoidal[F]): Ops[F, A] {
      type TypeClassType = ContravariantMonoidal[F]
    } =
      new Ops[F, A] {
        type TypeClassType = ContravariantMonoidal[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToContravariantMonoidalOps
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllContravariantMonoidalOps[F[_], A](
      target: F[A]
    )(implicit tc: ContravariantMonoidal[F]): AllOps[F, A] {
      type TypeClassType = ContravariantMonoidal[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = ContravariantMonoidal[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }

  /****************************************************************************/
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /****************************************************************************/

}

private[cats] class ContravariantMonoidalMonoid[F[_], A](f: ContravariantMonoidal[F])
    extends ContravariantSemigroupalSemigroup[F, A](f)
    with Monoid[F[A]] {
  def empty: F[A] = f.trivial
}
