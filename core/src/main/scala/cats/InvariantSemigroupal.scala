package cats

import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * [[InvariantSemigroupal]] is nothing more than something both invariant
 * and Semigroupal. It comes up enough to be useful, and composes well
 */
@implicitNotFound("Could not find an instance of InvariantSemigroupal for ${F}")
@typeclass trait InvariantSemigroupal[F[_]] extends Semigroupal[F] with Invariant[F] { self =>

  def composeApply[G[_]: Apply]: InvariantSemigroupal[λ[α => F[G[α]]]] =
    new ComposedInvariantApplySemigroupal[F, G] {
      def F = self
      def G = Apply[G]
    }

}

object InvariantSemigroupal extends SemigroupalArityFunctions {

  /**
   * Gives a `Semigroup` instance if A itself has a `Semigroup` instance.
   */
  def semigroup[F[_], A](implicit F: InvariantSemigroupal[F], A: Semigroup[A]): Semigroup[F[A]] =
    new InvariantSemigroupalSemigroup[F, A](F, A)

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[InvariantSemigroupal]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: InvariantSemigroupal[F]): InvariantSemigroupal[F] = instance

  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: InvariantSemigroupal[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A] with Semigroupal.AllOps[F, A] with Invariant.AllOps[F, A] {
    type TypeClassType <: InvariantSemigroupal[F]
  }
  trait ToInvariantSemigroupalOps extends Serializable {
    implicit def toInvariantSemigroupalOps[F[_], A](target: F[A])(implicit tc: InvariantSemigroupal[F]): Ops[F, A] {
      type TypeClassType = InvariantSemigroupal[F]
    } =
      new Ops[F, A] {
        type TypeClassType = InvariantSemigroupal[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToInvariantSemigroupalOps
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllInvariantSemigroupalOps[F[_], A](
      target: F[A]
    )(implicit tc: InvariantSemigroupal[F]): AllOps[F, A] {
      type TypeClassType = InvariantSemigroupal[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = InvariantSemigroupal[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}

private[cats] class InvariantSemigroupalSemigroup[F[_], A](f: InvariantSemigroupal[F], sg: Semigroup[A])
    extends Semigroup[F[A]] {
  def combine(a: F[A], b: F[A]): F[A] =
    InvariantSemigroupal.imap2(a, b)(sg.combine)(a => (a, a))(f, f)
}
