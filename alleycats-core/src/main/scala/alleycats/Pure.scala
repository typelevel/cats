package alleycats

import cats.{Applicative, FlatMap, Monad}
import simulacrum.typeclass
import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of Pure for ${F}")
@typeclass trait Pure[F[_]] extends Serializable {
  def pure[A](a: A): F[A]
}

object Pure {
  // Ideally this would be an exported subclass instance provided by Applicative
  implicit def applicativeIsPure[F[_]](implicit ev: Applicative[F]): Pure[F] =
    new Pure[F] {
      def pure[A](a: A): F[A] = ev.pure(a)
    }

  // Ideally this would be an instance exported to Monad
  implicit def pureFlatMapIsMonad[F[_]](implicit p: Pure[F], fm: FlatMap[F]): Monad[F] =
    new Monad[F] {
      def pure[A](a: A): F[A] = p.pure(a)
      override def map[A, B](fa: F[A])(f: A => B): F[B] = fm.map(fa)(f)
      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = fm.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: (A) => F[Either[A, B]]): F[B] = fm.tailRecM(a)(f)
    }

  /****************************************************************************/
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /****************************************************************************/
  /**
   * Summon an instance of [[Pure]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Pure[F]): Pure[F] = instance

  trait Ops[F[_], A] {
    type TypeClassType <: Pure[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A]
  trait ToPureOps {
    implicit def toPureOps[F[_], A](target: F[A])(implicit tc: Pure[F]): Ops[F, A] {
      type TypeClassType = Pure[F]
    } = new Ops[F, A] {
      type TypeClassType = Pure[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  object nonInheritedOps extends ToPureOps
  object ops {
    implicit def toAllPureOps[F[_], A](target: F[A])(implicit tc: Pure[F]): AllOps[F, A] {
      type TypeClassType = Pure[F]
    } = new AllOps[F, A] {
      type TypeClassType = Pure[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }

  /****************************************************************************/
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /****************************************************************************/

}
