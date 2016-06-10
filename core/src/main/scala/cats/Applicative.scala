package cats

import cats.std.list._
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

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    ap(pure(f))(fa)

  /**
    * Given `fa` and `n`, apply `fa` `n` times to construct an `F[List[A]]` value.
    */
  def replicateA[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def traverse[A, G[_], B](value: G[A])(f: A => F[B])(implicit G: Traverse[G]): F[G[B]] =
    G.traverse(value)(f)(this)

  def sequence[G[_], A](as: G[F[A]])(implicit G: Traverse[G]): F[G[A]] =
    G.sequence(as)(this)

  def compose[G[_]: Applicative]: Applicative[λ[α => F[G[α]]]] =
    new ComposedApplicative[F, G] {
      val F = self
      val G = Applicative[G]
    }
}
