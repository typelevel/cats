package cats
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

  def liftContravariant[A, B](f: A => B): F[B] => F[A] = contramap(_: F[B])(f)

  override def composeFunctor[G[_]: Functor]: Contravariant[λ[α => F[G[α]]]] =
    new ComposedContravariantCovariant[F, G] {
      val F = self
      val G = Functor[G]
    }
}
