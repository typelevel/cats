package cats

import simulacrum.typeclass

@typeclass trait Alternative[F[_]] extends Applicative[F] with MonoidK[F] { self =>

  /**
   * Compose this `Alternative` instance with an [[Applicative]] instance.
   */
  override def compose[G[_]](implicit GG: Applicative[G]): Alternative[λ[α => F[G[α]]]] =
    new CompositeAlternative[F, G] {
      implicit def F: Alternative[F] = self
      implicit def G: Applicative[G] = GG
    }
}

trait CompositeAlternative[F[_], G[_]]
  extends Alternative[λ[α => F[G[α]]]] with CompositeApplicative[F, G] with CompositeMonoidK[F, G] {

  implicit def F: Alternative[F]
  implicit def G: Applicative[G]
}
