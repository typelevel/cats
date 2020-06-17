package alleycats

import simulacrum.typeclass
import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of EmptyK for ${F}")
@typeclass trait EmptyK[F[_]] extends Serializable { self =>
  def empty[A]: F[A]

  def synthesize[A]: Empty[F[A]] =
    new Empty[F[A]] {
      def empty: F[A] = self.empty[A]
    }
}

object EmptyK {

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[EmptyK]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: EmptyK[F]): EmptyK[F] = instance

  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: EmptyK[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A]
  trait ToEmptyKOps extends Serializable {
    implicit def toEmptyKOps[F[_], A](target: F[A])(implicit tc: EmptyK[F]): Ops[F, A] {
      type TypeClassType = EmptyK[F]
    } =
      new Ops[F, A] {
        type TypeClassType = EmptyK[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToEmptyKOps
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllEmptyKOps[F[_], A](target: F[A])(implicit tc: EmptyK[F]): AllOps[F, A] {
      type TypeClassType = EmptyK[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = EmptyK[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
