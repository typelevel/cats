package cats
import simulacrum.typeclass
import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of Distributive for ${F}")
@typeclass trait Distributive[F[_]] extends Functor[F] { self =>

  /**
   * Given a function which returns a distributive `F`, apply that value across the structure G.
   */
  def distribute[G[_]: Functor, A, B](ga: G[A])(f: A => F[B]): F[G[B]]

  /**
   * Given a Functor G which wraps some distributive F, distribute F across the G.
   */
  def cosequence[G[_]: Functor, A](ga: G[F[A]]): F[G[A]] = distribute(ga)(identity)

  // Distributive composes
  def compose[G[_]](implicit G0: Distributive[G]): Distributive[λ[α => F[G[α]]]] =
    new ComposedDistributive[F, G] {
      implicit def F = self
      implicit def G = G0
    }
}

object Distributive {

  /****************************************************************************/
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /****************************************************************************/
  /**
   * Summon an instance of [[Distributive]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Distributive[F]): Distributive[F] = instance

  trait Ops[F[_], A] {
    type TypeClassType <: Distributive[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[F[_], A] extends Ops[F, A] with Functor.AllOps[F, A] {
    type TypeClassType <: Distributive[F]
  }
  trait ToDistributiveOps {
    implicit def toDistributiveOps[F[_], A](target: F[A])(implicit tc: Distributive[F]): Ops[F, A] {
      type TypeClassType = Distributive[F]
    } =
      new Ops[F, A] {
        type TypeClassType = Distributive[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  object nonInheritedOps extends ToDistributiveOps
  object ops {
    implicit def toAllDistributiveOps[F[_], A](target: F[A])(implicit tc: Distributive[F]): AllOps[F, A] {
      type TypeClassType = Distributive[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = Distributive[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }

  /****************************************************************************/
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /****************************************************************************/

}
