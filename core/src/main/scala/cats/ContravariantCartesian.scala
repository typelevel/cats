package cats

import functor.Contravariant
import simulacrum.typeclass

/**
 * [[ContravariantCartesian]] is nothing more than something both contravariant
 * and Cartesian. It comes up enough to be useful, and composes well
 */
@typeclass trait ContravariantCartesian[F[_]] extends Cartesian[F] with Contravariant[F] { self =>
  override def composeFunctor[G[_]: Functor]: ContravariantCartesian[λ[α => F[G[α]]]] =
    new ComposedCartesian[F, G] {
      def F = self
      def G = Functor[G]
    }
}

object ContravariantCartesian extends KernelContravariantCartesianInstances

private[cats] sealed trait KernelContravariantCartesianInstances extends ContravariantCartesian1 {
  implicit val catsContravariantCartesianEq: ContravariantCartesian[Eq] = new ContravariantCartesian[Eq] {
    def contramap[A, B](fa: Eq[A])(fn: B => A): Eq[B] = fa.on(fn)
    def product[A, B](fa: Eq[A], fb: Eq[B]): Eq[(A, B)] =
      Eq.instance { (left, right) => fa.eqv(left._1, right._1) && fb.eqv(left._2, right._2) }
  }
}

private[cats] sealed trait ContravariantCartesian1 {
  implicit def catsContravariantCartesianFromEach[F[_]](implicit contra: Contravariant[F], cart: Cartesian[F]): ContravariantCartesian[F] = new ContravariantCartesian[F] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = cart.product(fa, fb)
    def contramap[A, B](fa: F[A])(fn: B => A): F[B] = contra.contramap(fa)(fn)
  }
}
