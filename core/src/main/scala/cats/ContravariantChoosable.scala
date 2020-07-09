package cats

import cats.data.INothing
import simulacrum.typeclass

/**
 * Contravariant version of an Additive Monoidal.
 *
 * Must obey the laws defined in cats.laws.ContravariantChoosableLaws.
 */
@typeclass trait ContravariantChoosable[F[_]] extends InvariantChoosable[F] with ContravariantChoice[F] {
  def lose[A](f: A => INothing): F[A] =
    contravariant.contramap(zero)(f)
}

object ContravariantChoosable {
  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[ContravariantChoosable]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: ContravariantChoosable[F]): ContravariantChoosable[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllContravariantChoosableOps[F[_], A](
      target: F[A]
    )(implicit tc: ContravariantChoosable[F]): AllOps[F, A] {
      type TypeClassType = ContravariantChoosable[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = ContravariantChoosable[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: ContravariantChoosable[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A] with InvariantChoosable.AllOps[F, A] with ContravariantChoice.AllOps[F, A] {
    type TypeClassType <: ContravariantChoosable[F]
  }
  trait ToContravariantChoosableOps extends Serializable {
    implicit def toContravariantChoosableOps[F[_], A](target: F[A])(implicit tc: ContravariantChoosable[F]): Ops[F, A] {
      type TypeClassType = ContravariantChoosable[F]
    } =
      new Ops[F, A] {
        type TypeClassType = ContravariantChoosable[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToContravariantChoosableOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
