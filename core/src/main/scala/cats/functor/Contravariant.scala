package cats
package functor

import simulacrum.typeclass

/**
 * Must obey the laws defined in cats.laws.ContravariantLaws.
 */
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

  override def composeFunctor[G[_]: Functor]: Contravariant[λ[α => F[G[α]]]] =
    new ComposedContravariantCovariant[F, G] {
      val F = self
      val G = Functor[G]
    }
}

object Contravariant extends KernelContravariantInstances

/**
 * Contravariant instances for types that are housed in cats.kernel and therefore
 * can't have instances for this type class in their companion objects.
 */
private[functor] sealed trait KernelContravariantInstances {
  implicit def catsFunctorContravariantForEq: Contravariant[Eq] =
    ContravariantCartesian.catsContravariantCartesianEq

  implicit val catsFunctorContravariantForPartialOrder: Contravariant[PartialOrder] =
    new Contravariant[PartialOrder] {
      /** Derive a `PartialOrder` for `B` given a `PartialOrder[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: PartialOrder[A])(f: B => A): PartialOrder[B] = fa.on(f)
    }

  implicit val catsFunctorContravariantForOrder: Contravariant[Order] =
    new Contravariant[Order] {
      /** Derive an `Order` for `B` given an `Order[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: Order[A])(f: B => A): Order[B] = fa.on(f)
    }

  implicit val catsFunctorContravariantForHash: Contravariant[Hash] =
    new Contravariant[Hash] {
      /** Derive an `Hash` for `B` given an `Hash[A]` and a function `B => A`.
       *
       * Note: resulting instances are law-abiding only when the functions used are injective (represent a one-to-one mapping)
       */
      def contramap[A, B](fa: Hash[A])(f: B => A): Hash[B] = Hash.by(f)(fa)
    }

}
