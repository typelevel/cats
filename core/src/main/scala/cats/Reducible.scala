package cats

import simulacrum._

import Fold.{Return, Pass, Continue}

/**
 * Data structures that can be reduced to a summary value.
 *
 * `Reducible` is like a non-empty `Foldable`. In addition to the fold
 * methods it provides reduce methods which do not require an initial
 * value.
 *
 * In addition to the methods needed by `Foldable`, `Reducible` is
 * implemented in terms of two methods:
 *
 *  - `reduceLeftTo(fa)(f)(g)` eagerly reduces with an additional mapping function
 *  - `reduceRightTo(fa)(f)(g)` lazily reduces with an additional mapping function
 */
@typeclass trait Reducible[F[_]] extends Foldable[F] { self =>

  /**
   * Left-associative reduction on `F` using the function `f`.
   *
   * Implementations should override this method when possible.
   */
  def reduceLeft[A](fa: F[A])(f: (A, A) => A): A =
    reduceLeftTo(fa)(identity)(f)

  /**
   * Right-associative reduction on `F` using the function `f`.
   */
  def reduceRight[A](fa: F[A])(f: A => Fold[A]): Lazy[A] =
    reduceRightTo(fa)(identity)(f)

  /**
   * Reduce a `F[A]` value using the given `Semigroup[A]`.
   */
  def reduce[A](fa: F[A])(implicit A: Semigroup[A]): A =
    reduceLeft(fa)(A.combine)

  /**
   * Reduce a `F[G[A]]` value using `SemigroupK[G]`, a universal
   * semigroup for `G[_]`.
   *
   * This method is a generalization of `reduce`.
   */
  def reduceK[G[_], A](fga: F[G[A]])(implicit G: SemigroupK[G]): G[A] =
    reduce(fga)(G.algebra)

  /**
   * Apply `f` to each element of `fa` and combine them using the
   * given `Semigroup[B]`.
   */
  def reduceMap[A, B](fa: F[A])(f: A => B)(implicit B: Semigroup[B]): B =
    reduceLeftTo(fa)(f)((b, a) => B.combine(b, f(a)))

  /**
   * Apply `f` to the "initial element" of `fa` and combine it with
   * every other value using the given function `g`.
   */
  def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B

  /**
   * Overriden from Foldable[_] for efficiency.
   */
  override def reduceLeftToOption[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): Option[B] =
    Some(reduceLeftTo(fa)(f)(g))

  /**
   * Apply `f` to the "initial element" of `fa` and lazily combine it
   * with every other value using the given function `g`.
   */
  def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: A => Fold[B]): Lazy[B]

  /**
   * Overriden from `Foldable[_]` for efficiency.
   */
  override def reduceRightToOption[A, B](fa: F[A])(f: A => B)(g: A => Fold[B]): Lazy[Option[B]] =
    Lazy(Some(reduceRightTo(fa)(f)(g).value))

  /**
   * Traverse `F[A]` using `Apply[G]`.
   *
   * `A` values will be mapped into `G[B]` and combined using
   * `Applicative#map2`.
   *
   * This method does the same thing as `Foldable#traverse_`.  The
   * difference is that we only need `Apply[G]` here, since we don't
   * need to call `Applicative#pure` for a starting value.
   */
  def traverse1_[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Apply[G]): G[Unit] =
    G.map(reduceLeftTo(fa)(f)((x, y) => G.map2(x, f(y))((_, b) => b)))(_ => ())

  /**
   * Sequence `F[G[A]]` using `Apply[G]`.
   *
   * This method is similar to `Foldable#sequence_`. The difference is
   * that we only need `Apply[G]` here, since we don't need to call
   * `Applicative#pure` for a starting value.
   */
  def sequence1_[G[_], A, B](fga: F[G[A]])(implicit G: Apply[G]): G[Unit] =
    G.map(reduceLeft(fga)((x, y) => G.map2(x, y)((_, b) => b)))(_ => ())

  /**
   * Compose two `Reducible` instances into a new one.
   */
  def compose[G[_]](implicit GG: Reducible[G]): Reducible[λ[α => F[G[α]]]] =
    new CompositeReducible[F, G] {
      implicit def F: Reducible[F] = self
      implicit def G: Reducible[G] = GG
    }
}


/**
 * This class composes two `Reducible` instances to provide an
 * instance for the nested types.
 *
 * In other words, given a `Reducible[F]` instance (which can reduce
 * `F[A]`) and a `Reducible[G]` instance (which can reduce `G[A]`
 * values), this class is able to reduce `F[G[A]]` values.
 */
trait CompositeReducible[F[_], G[_]] extends Reducible[λ[α => F[G[α]]]] with CompositeFoldable[F, G] {
  implicit def F: Reducible[F]
  implicit def G: Reducible[G]

  override def reduceLeftTo[A, B](fga: F[G[A]])(f: A => B)(g: (B, A) => B): B = {
    def toB(ga: G[A]): B = G.reduceLeftTo(ga)(f)(g)
    F.reduceLeftTo(fga)(toB) { (b, ga) =>
      G.foldLeft(ga, b)(g)
    }
  }

  override def reduceRightTo[A, B](fga: F[G[A]])(f: A => B)(g: A => Fold[B]): Lazy[B] = {
    def toB(ga: G[A]): B = G.reduceRightTo(ga)(f)(g).value
    F.reduceRightTo(fga)(toB) { ga =>
      Fold.Continue(b => G.foldRight(ga, Lazy(b))(g).value)
    }
  }
}


/**
 * This class defines a `Reducible[F]` in terms of a `Foldable[G]`
 * together with a `split method, `F[A]` => `(A, G[A])`.
 *
 * This class can be used on any type where the first value (`A`) and
 * the "rest" of the values (`G[A]`) can be easily found.
 */
abstract class NonEmptyReducible[F[_], G[_]](implicit G: Foldable[G]) extends Reducible[F] {
  def split[A](fa: F[A]): (A, G[A])

  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = {
    val (a, ga) = split(fa)
    G.foldLeft(ga, f(b, a))(f)
  }

  def partialFold[A, B](fa: F[A])(f: A => Fold[B]): Fold[B] = {
    val (a, ga) = split(fa)
    def right: Fold[B] = G.partialFold(ga)(f)
    f(a) match {
      case Return(b) => Return(b)
      case Continue(g) => right compose g
      case _ => right
    }
  }

  def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B = {
    val (a, ga) = split(fa)
    G.foldLeft(ga, f(a))((b, a) => g(b, a))
  }

  def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: A => Fold[B]): Lazy[B] = {
    val (a, ga) = split(fa)
    Lazy {
      G.reduceRightToOption(ga)(f)(g).value match {
        case None => f(a)
        case Some(b) => g(a).complete(Lazy.eager(b))
      }
    }
  }
}
