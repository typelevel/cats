package cats
import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * Must obey the laws defined in cats.laws.ContravariantLaws.
 */
@implicitNotFound("Could not find an instance of Contravariant for ${F}")
@typeclass trait Contravariant[F[_]] extends Invariant[F] { self =>
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
  override def imap[A, B](fa: F[A])(f: A => B)(fi: B => A): F[B] = contramap(fa)(fi)

  def compose[G[_]: Contravariant]: Functor[λ[α => F[G[α]]]] =
    new ComposedContravariant[F, G] {
      val F = self
      val G = Contravariant[G]
    }

  /**
   * Lifts natural subtyping contravariance of contravariant Functors.
   * could be implemented as contramap(identity), but the Functor laws say this is equivalent
   */
  def narrow[A, B <: A](fa: F[A]): F[B] = fa.asInstanceOf[F[B]]

  def liftContravariant[A, B](f: A => B): F[B] => F[A] = contramap(_: F[B])(f)

  override def composeFunctor[G[_]: Functor]: Contravariant[λ[α => F[G[α]]]] =
    new ComposedContravariantCovariant[F, G] {
      val F = self
      val G = Functor[G]
    }
}

object Contravariant {

  /****************************************************************************
   * THE REST OF THIS OBJECT IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!! *
   ****************************************************************************/
  /**
   * Summon an instance of [[Contravariant]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Contravariant[F]): Contravariant[F] = instance

  trait Ops[F[_], A] {
    type TypeClassType <: Contravariant[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def contramap[B](f: B => A): F[B] = typeClassInstance.contramap[A, B](self)(f)
    def narrow[B <: A]: F[B] = typeClassInstance.narrow[A, B](self)
  }
  trait AllOps[F[_], A] extends Ops[F, A] with Invariant.AllOps[F, A] {
    type TypeClassType <: Contravariant[F]
  }
  trait ToContravariantOps {
    implicit def toContravariantOps[F[_], A](target: F[A])(implicit tc: Contravariant[F]): Ops[F, A] {
      type TypeClassType = Contravariant[F]
    } = new Ops[F, A] {
      type TypeClassType = Contravariant[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  object nonInheritedOps extends ToContravariantOps
  object ops {
    implicit def toAllContravariantOps[F[_], A](target: F[A])(implicit tc: Contravariant[F]): AllOps[F, A] {
      type TypeClassType = Contravariant[F]
    } = new AllOps[F, A] {
      type TypeClassType = Contravariant[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
}
