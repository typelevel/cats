package cats

import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * [[ContravariantSemigroupal]] is nothing more than something both contravariant
 * and Semigroupal. It comes up enough to be useful, and composes well
 */
@implicitNotFound("Could not find an instance of ContravariantSemigroupal for ${F}")
@typeclass trait ContravariantSemigroupal[F[_]] extends InvariantSemigroupal[F] with Contravariant[F] { self =>
  override def composeFunctor[G[_]: Functor]: ContravariantSemigroupal[λ[α => F[G[α]]]] =
    new ComposedSemigroupal[F, G] {
      def F = self
      def G = Functor[G]
    }

}

object ContravariantSemigroupal extends SemigroupalArityFunctions {
  def semigroup[F[_], A](implicit f: ContravariantSemigroupal[F]): Semigroup[F[A]] =
    new ContravariantSemigroupalSemigroup[F, A](f)

  /****************************************************************************/
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /****************************************************************************/

  /**
   * Summon an instance of [[ContravariantSemigroupal]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: ContravariantSemigroupal[F]): ContravariantSemigroupal[F] = instance

  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: ContravariantSemigroupal[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A] with InvariantSemigroupal.AllOps[F, A] with Contravariant.AllOps[F, A] {
    type TypeClassType <: ContravariantSemigroupal[F]
  }
  trait ToContravariantSemigroupalOps extends Serializable {
    implicit def toContravariantSemigroupalOps[F[_], A](
      target: F[A]
    )(implicit tc: ContravariantSemigroupal[F]): Ops[F, A] {
      type TypeClassType = ContravariantSemigroupal[F]
    } =
      new Ops[F, A] {
        type TypeClassType = ContravariantSemigroupal[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToContravariantSemigroupalOps
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllContravariantSemigroupalOps[F[_], A](
      target: F[A]
    )(implicit tc: ContravariantSemigroupal[F]): AllOps[F, A] {
      type TypeClassType = ContravariantSemigroupal[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = ContravariantSemigroupal[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }

  /****************************************************************************/
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /****************************************************************************/

}

private[cats] class ContravariantSemigroupalSemigroup[F[_], A](f: ContravariantSemigroupal[F]) extends Semigroup[F[A]] {
  def combine(a: F[A], b: F[A]): F[A] =
    ContravariantSemigroupal.contramap2(a, b)((a: A) => (a, a))(f, f)
}
