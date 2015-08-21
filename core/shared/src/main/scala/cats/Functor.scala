package cats

import simulacrum.typeclass
import functor.Contravariant

/**
 * Functor.
 *
 * The name is short for "covariant functor".
 *
 * Must obey the laws defined in cats.laws.FunctorLaws.
 */
@typeclass trait Functor[F[_]] extends functor.Invariant[F] { self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def imap[A, B](fa: F[A])(f: A => B)(fi: B => A): F[B] = map(fa)(f)

  /**
   * Compose this functor F with a functor G to produce a composite
   * Functor on G[F[_]], with a map method which uses an A => B to
   * map a G[F[A]] to a G[F[B]].
   */
  def compose[G[_]](implicit GG: Functor[G]): Functor[Lambda[X => F[G[X]]]] = new Functor.Composite[F, G] {
    def F: Functor[F] = self
    def G: Functor[G] = GG
  }

  /**
   * Compose this functor F with a Contravariant Functor G to produce a new Contravariant Functor
   * on F[G[_]].
   */
  override def composeWithContravariant[G[_]](implicit GG: Contravariant[G]): Contravariant[Lambda[X => F[G[X]]]] =
    new Functor.ContravariantComposite[F, G] {
      def F: Functor[F] = self
      def G: Contravariant[G] = GG
    }

  override def composeWithFunctor[G[_]: Functor]: Functor[Lambda[X => F[G[X]]]] = compose[G]


  // derived methods

  /**
   * Lift a function f to operate on Functors
   */
  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)

  /**
   * Empty the fa of the values, preserving the structure
   */
  def void[A](fa: F[A]): F[Unit] = map(fa)(_ => ())

  /**
   * Tuple the values in fa with the result of applying a function
   * with the value
   */
  def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)] = map(fa)(a => a -> f(a))

  /**
   * Replaces the `A` value in `F[A]` with the supplied value.
   */
  def as[A, B](fa: F[A], b: B): F[B] = map(fa)(_ => b)
}

object Functor {
  trait Composite[F[_], G[_]] extends Functor[Lambda[X => F[G[X]]]] {
    def F: Functor[F]
    def G: Functor[G]

    override def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] =
      F.map(fa)(G.lift(f))
  }

  trait ContravariantComposite[F[_], G[_]] extends Contravariant[Lambda[X => F[G[X]]]] {
    def F: Functor[F]
    def G: Contravariant[G]

    override def contramap[A, B](fa: F[G[A]])(f: B => A): F[G[B]] =
      F.map(fa)(ga => G.contramap(ga)(f))
  }
}
