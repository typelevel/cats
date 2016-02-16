package cats

import simulacrum.typeclass

/**
 * Applicative functor.
 *
 * Allows application of a function in an Applicative context to a value in an Applicative context
 *
 * See: [[https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf The Essence of the Iterator Pattern]]
 * Also: [[http://staff.city.ac.uk/~ross/papers/Applicative.pdf Applicative programming with effects]]
 *
 * Must obey the laws defined in cats.laws.ApplicativeLaws.
 */
@typeclass trait Applicative[F[_]] extends Apply[F] { self =>

  /**
   * `pure` lifts any value into the Applicative Functor.
   *
   * Applicative[Option].pure(10) = Some(10)
   */
  def pure[A](x: A): F[A]

  /**
   * `pureEval` lifts any value into the Applicative Functor.
   *
   * This variant supports optional laziness.
   */
  def pureEval[A](x: Eval[A]): F[A] = pure(x.value)

  /**
   * Two sequentially dependent Applicatives can be composed.
   *
   * The composition of Applicatives `F` and `G`, `F[G[x]]`, is also an Applicative
   *
   * Applicative[Option].compose[List].pure(10) = Some(List(10))
   */
  def compose[G[_]](implicit GG : Applicative[G]): Applicative[λ[α => F[G[α]]]] =
    new CompositeApplicative[F,G] {
      implicit def F: Applicative[F] = self
      implicit def G: Applicative[G] = GG

    }

  def traverse[A, G[_], B](value: G[A])(f: A => F[B])(implicit G: Traverse[G]): F[G[B]] =
    G.traverse(value)(f)(this)

  def sequence[G[_], A](as: G[F[A]])(implicit G: Traverse[G]): F[G[A]] =
    G.sequence(as)(this)

}

trait CompositeApplicative[F[_],G[_]]
    extends Applicative[λ[α => F[G[α]]]] with CompositeApply[F,G] {

  implicit def F: Applicative[F]
  implicit def G: Applicative[G]

  def pure[A](a: A): F[G[A]] = F.pure(G.pure(a))
}
