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
  def semigroup[F[_], A](implicit f: InvariantSemigroupal[F], sg: Semigroup[A]): Semigroup[F[A]] =
    new InvariantSemigroupalSemigroup[F, A](f, sg)

  implicit def catsSemigroupalForMonoid: InvariantSemigroupal[Monoid] = new InvariantSemigroupal[Monoid] {
    def product[A, B](fa: Monoid[A], fb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
      val empty = fa.empty -> fb.empty
      def combine(x: (A, B), y: (A, B)): (A, B) = fa.combine(x._1, y._1) -> fb.combine(x._2, y._2)
    }

    def imap[A, B](fa: Monoid[A])(f: A => B)(g: B => A): Monoid[B] = new Monoid[B] {
      def empty: B = f(fa.empty)

      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
    }
  }
}

private[cats] class InvariantSemigroupalSemigroup[F[_], A](f: InvariantSemigroupal[F], sg: Semigroup[A]) extends Semigroup[F[A]] {
  def combine(a: F[A], b: F[A]): F[A] =
    InvariantSemigroupal.imap2(a, b)(sg.combine)(a => (a, a))(f, f)
}
