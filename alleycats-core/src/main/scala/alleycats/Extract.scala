package alleycats

import cats.{CoflatMap, Comonad}

import simulacrum.typeclass
import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of Extract for ${F}")
@typeclass trait Extract[F[_]] extends Serializable {
  def extract[A](fa: F[A]): A
}

object Extract {
  // Ideally this would be an exported subclass instance provided by Comonad
  implicit def comonadIsExtract[F[_]](implicit ev: Comonad[F]): Extract[F] =
    new Extract[F] {
      def extract[A](fa: F[A]): A = ev.extract(fa)
    }

  // Ideally this would be an instance exported to Comonad
  implicit def extractCoflatMapIsComonad[F[_]](implicit e: Extract[F], cf: CoflatMap[F]): Comonad[F] =
    new Comonad[F] {
      def extract[A](fa: F[A]): A = e.extract(fa)
      override def map[A, B](fa: F[A])(f: A => B): F[B] = cf.map(fa)(f)
      def coflatMap[A, B](fa: F[A])(f: F[A] => B): F[B] = cf.coflatMap(fa)(f)
    }

  /****************************************************************************/
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /****************************************************************************/

  /**
   * Summon an instance of [[Extract]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Extract[F]): Extract[F] = instance

  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: Extract[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def extract: A = typeClassInstance.extract[A](self)
  }
  trait AllOps[F[_], A] extends Ops[F, A]
  trait ToExtractOps extends Serializable {
    implicit def toExtractOps[F[_], A](target: F[A])(implicit tc: Extract[F]): Ops[F, A] {
      type TypeClassType = Extract[F]
    } =
      new Ops[F, A] {
        type TypeClassType = Extract[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToExtractOps
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllExtractOps[F[_], A](target: F[A])(implicit tc: Extract[F]): AllOps[F, A] {
      type TypeClassType = Extract[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = Extract[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }

  /****************************************************************************/
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /****************************************************************************/

}
