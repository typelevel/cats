package cats

import simulacrum.typeclass

/**
  * [[InvariantSemigroupal]] is nothing more than something both invariant
  * and Semigroupal. It comes up enough to be useful, and composes well
  */
@typeclass trait InvariantSemigroupal[F[_]] extends Semigroupal[F] with Invariant[F] { self =>

   def composeApply[G[_]: Apply]: InvariantSemigroupal[λ[α => F[G[α]]]] =
     new ComposedInvariantApplySemigroupal[F, G] {
       def F = self
       def G = Apply[G]
     }

}

object InvariantSemigroupal extends SemigroupalArityFunctions {
  /**
    * Gives a `Semigroup` instance if A itself has a `Semigroup` instance.
    */
  def semigroup[F[_], A](implicit F: InvariantSemigroupal[F], A: Semigroup[A]): Semigroup[F[A]] =
    new InvariantSemigroupalSemigroup[F, A](F, A)
}

private[cats] class InvariantSemigroupalSemigroup[F[_], A](f: InvariantSemigroupal[F], sg: Semigroup[A]) extends Semigroup[F[A]] {
  def combine(a: F[A], b: F[A]): F[A] =
    InvariantSemigroupal.imap2(a, b)(sg.combine)(a => (a, a))(f, f)
}
