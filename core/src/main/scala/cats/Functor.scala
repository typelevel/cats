package cats

import simulacrum.typeclass

/**
 * Functor.
 *
 * The name is short for "covariant functor".
 *
 * Must obey the laws defined in cats.laws.FunctorLaws.
 */
@typeclass trait Functor[F[_]] extends Invariant[F] { self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]

  override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] = map(fa)(f)

  // derived methods

  /**
    * Alias for [[map]], since [[map]] can't be injected as syntax if
    * the implementing type already had a built-in `.map` method.
    *
    * Example:
    * {{{
    * scala> import cats.implicits._
    *
    * scala> val m: Map[Int, String] = Map(1 -> "hi", 2 -> "there", 3 -> "you")
    *
    * scala> m.fmap(_ ++ "!")
    * res0: Map[Int,String] = Map(1 -> hi!, 2 -> there!, 3 -> you!)
    * }}}
    */
  final def fmap[A, B](fa: F[A])(f: A => B): F[B] = map(fa)(f)

  /**
   * Lifts natural subtyping covariance of covariant Functors.
   *
   * NOTE: In certain (perhaps contrived) situations that rely on universal
   * equality this can result in a `ClassCastException`, because it is
   * implemented as a type cast. It could be implemented as `map(identity)`, but
   * according to the functor laws, that should be equal to `fa`, and a type
   * cast is often much more performant.
   * See [[https://github.com/typelevel/cats/issues/1080#issuecomment-225892635 this example]]
   * of `widen` creating a `ClassCastException`.
   */
  def widen[A, B >: A](fa: F[A]): F[B] = fa.asInstanceOf[F[B]]

  /**
   * Lift a function f to operate on Functors
   */
  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)

  /**
   * Empty the fa of the values, preserving the structure
   */
  def void[A](fa: F[A]): F[Unit] = as(fa, ())

  /**
   * Tuple the values in fa with the result of applying a function
   * with the value
   */
  def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)] = map(fa)(a => a -> f(a))

  /**
   * Replaces the `A` value in `F[A]` with the supplied value.
   */
  def as[A, B](fa: F[A], b: B): F[B] = map(fa)(_ => b)

  /**
    * Tuples the `A` value in `F[A]` with the supplied `B` value, with the `B` value on the left.
    */
  def tupleLeft[A, B](fa: F[A], b: B): F[(B, A)] = map(fa)(a => (b, a))

  /**
    * Tuples the `A` value in `F[A]` with the supplied `B` value, with the `B` value on the right.
    */
  def tupleRight[A, B](fa: F[A], b: B): F[(A, B)] = map(fa)(a => (a, b))

  def compose[G[_]: Functor]: Functor[λ[α => F[G[α]]]] =
    new ComposedFunctor[F, G] {
      val F = self
      val G = Functor[G]
    }

  override def composeContravariant[G[_]: Contravariant]: Contravariant[λ[α => F[G[α]]]] =
    new ComposedCovariantContravariant[F, G] {
      val F = self
      val G = Contravariant[G]
    }
}
