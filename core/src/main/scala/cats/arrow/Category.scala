package cats
package arrow

import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * Must obey the laws defined in cats.laws.CategoryLaws.
 */
@implicitNotFound("Could not find an instance of Category for ${F}")
@typeclass trait Category[F[_, _]] extends Compose[F] { self =>

  def id[A]: F[A, A]

  override def algebraK: MonoidK[λ[α => F[α, α]]] =
    new MonoidK[λ[α => F[α, α]]] {
      def empty[A]: F[A, A] = id
      def combineK[A](f1: F[A, A], f2: F[A, A]): F[A, A] = self.compose(f1, f2)
    }

  override def algebra[A]: Monoid[F[A, A]] =
    new Monoid[F[A, A]] {
      def empty: F[A, A] = id
      def combine(f1: F[A, A], f2: F[A, A]): F[A, A] = self.compose(f1, f2)
    }
}

object Category {

  /****************************************************************************/
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /****************************************************************************/
  /**
   * Summon an instance of [[Category]] for `F`.
   */
  @inline def apply[F[_, _]](implicit instance: Category[F]): Category[F] = instance

  trait Ops[F[_, _], A, B] {
    type TypeClassType <: Category[F]
    def self: F[A, B]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_, _], A, B] extends Ops[F, A, B] with Compose.AllOps[F, A, B] {
    type TypeClassType <: Category[F]
  }
  trait ToCategoryOps {
    implicit def toCategoryOps[F[_, _], A, B](target: F[A, B])(implicit tc: Category[F]): Ops[F, A, B] {
      type TypeClassType = Category[F]
    } = new Ops[F, A, B] {
      type TypeClassType = Category[F]
      val self: F[A, B] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  object nonInheritedOps extends ToCategoryOps
  object ops {
    implicit def toAllCategoryOps[F[_, _], A, B](target: F[A, B])(implicit tc: Category[F]): AllOps[F, A, B] {
      type TypeClassType = Category[F]
    } = new AllOps[F, A, B] {
      type TypeClassType = Category[F]
      val self: F[A, B] = target
      val typeClassInstance: TypeClassType = tc
    }
  }

  /****************************************************************************/
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /****************************************************************************/

}
