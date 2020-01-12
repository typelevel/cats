package cats

import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * `UnorderedTraverse` is like a `Traverse` for unordered containers.
 */
@implicitNotFound("Could not find an instance of UnorderedTraverse for ${F}")
@typeclass trait UnorderedTraverse[F[_]] extends UnorderedFoldable[F] {
  def unorderedTraverse[G[_]: CommutativeApplicative, A, B](sa: F[A])(f: A => G[B]): G[F[B]]

  def unorderedSequence[G[_]: CommutativeApplicative, A](fga: F[G[A]]): G[F[A]] =
    unorderedTraverse(fga)(identity)
}

object UnorderedTraverse {

  /****************************************************************************
   * THE REST OF THIS OBJECT IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!! *
   ****************************************************************************/
  /**
   * Summon an instance of [[UnorderedTraverse]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: UnorderedTraverse[F]): UnorderedTraverse[F] = instance

  trait Ops[F[_], A] {
    type TypeClassType <: UnorderedTraverse[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def unorderedTraverse[G[_], B](f: A => G[B])(implicit ev$1: CommutativeApplicative[G]): G[F[B]] =
      typeClassInstance.unorderedTraverse[G, A, B](self)(f)
    def unorderedSequence[G[_], B](implicit ev$1: A <:< G[B], ev$2: CommutativeApplicative[G]): G[F[B]] =
      typeClassInstance.unorderedSequence[G, B](self.asInstanceOf[F[G[B]]])
  }
  trait AllOps[F[_], A] extends Ops[F, A] with UnorderedFoldable.AllOps[F, A] {
    type TypeClassType <: UnorderedTraverse[F]
  }
  trait ToUnorderedTraverseOps {
    implicit def toUnorderedTraverseOps[F[_], A](target: F[A])(implicit tc: UnorderedTraverse[F]): Ops[F, A] {
      type TypeClassType = UnorderedTraverse[F]
    } = new Ops[F, A] {
      type TypeClassType = UnorderedTraverse[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  object nonInheritedOps extends ToUnorderedTraverseOps
  object ops {
    implicit def toAllUnorderedTraverseOps[F[_], A](target: F[A])(implicit tc: UnorderedTraverse[F]): AllOps[F, A] {
      type TypeClassType = UnorderedTraverse[F]
    } = new AllOps[F, A] {
      type TypeClassType = UnorderedTraverse[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
}
