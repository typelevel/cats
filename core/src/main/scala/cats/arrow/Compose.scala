package cats
package arrow

import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * Must obey the laws defined in cats.laws.ComposeLaws.
 *
 * Here's how you can use `>>>` and `<<<`
 * Example:
 * {{{
 * scala> import cats.implicits._
 * scala> val f : Int => Int = (_ + 1)
 * scala> val g : Int => Int = (_ * 100)
 * scala> (f >>> g)(3)
 * res0: Int = 400
 * scala> (f <<< g)(3)
 * res1: Int = 301
 * }}}
 */
@implicitNotFound("Could not find an instance of Compose for ${F}")
@typeclass trait Compose[F[_, _]] extends Serializable { self =>

  @simulacrum.op("<<<", alias = true)
  def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C]

  @simulacrum.op(">>>", alias = true)
  def andThen[A, B, C](f: F[A, B], g: F[B, C]): F[A, C] =
    compose(g, f)

  def algebraK: SemigroupK[λ[α => F[α, α]]] =
    new SemigroupK[λ[α => F[α, α]]] {
      def combineK[A](f1: F[A, A], f2: F[A, A]): F[A, A] = self.compose(f1, f2)
    }

  def algebra[A]: Semigroup[F[A, A]] =
    new Semigroup[F[A, A]] {
      def combine(f1: F[A, A], f2: F[A, A]): F[A, A] = self.compose(f1, f2)
    }
}

object Compose {
  implicit def catsInstancesForFunction1: ArrowChoice[Function1] with CommutativeArrow[Function1] =
    cats.instances.function.catsStdInstancesForFunction1
  implicit def catsComposeForMap: Compose[Map] = cats.instances.map.catsStdComposeForMap

  /****************************************************************************/
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /****************************************************************************/

  /**
   * Summon an instance of [[Compose]] for `F`.
   */
  @inline def apply[F[_, _]](implicit instance: Compose[F]): Compose[F] = instance

  trait Ops[F[_, _], A, B] extends Serializable {
    type TypeClassType <: Compose[F]
    def self: F[A, B]
    val typeClassInstance: TypeClassType
    def compose[C](g: F[C, A]): F[C, B] = typeClassInstance.compose[C, A, B](self, g)
    def <<<[C](g: F[C, A]): F[C, B] = typeClassInstance.compose[C, A, B](self, g)
    def andThen[C](g: F[B, C]): F[A, C] = typeClassInstance.andThen[A, B, C](self, g)
    def >>>[C](g: F[B, C]): F[A, C] = typeClassInstance.andThen[A, B, C](self, g)
  }
  trait AllOps[F[_, _], A, B] extends Ops[F, A, B]
  trait ToComposeOps extends Serializable {
    implicit def toComposeOps[F[_, _], A, B](target: F[A, B])(implicit tc: Compose[F]): Ops[F, A, B] {
      type TypeClassType = Compose[F]
    } =
      new Ops[F, A, B] {
        type TypeClassType = Compose[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToComposeOps
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllComposeOps[F[_, _], A, B](target: F[A, B])(implicit tc: Compose[F]): AllOps[F, A, B] {
      type TypeClassType = Compose[F]
    } =
      new AllOps[F, A, B] {
        type TypeClassType = Compose[F]
        val self: F[A, B] = target
        val typeClassInstance: TypeClassType = tc
      }
  }

  /****************************************************************************/
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /****************************************************************************/

}
