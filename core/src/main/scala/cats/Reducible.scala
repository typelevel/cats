package cats

import simulacrum._

import Reducible.EmptyReduceError
import Fold.{Return, Pass, Continue}

/**
 * Data structures that can be reduced to a summary value.
 *
 * `Reducible` is like a non-empty `Foldable`. In addition to the fold
 * methods it provides reduce methods which do not require an initial
 * value.
 *
 * In addition to the methods needed by `Foldable`, `Reducible` is
 * implemented in terms of three methods:
 *
 *  - `reduceLeft(fa)(f)` eagerly reduces `fa` from left-to-right
 *  - `reduceRight(fa)(f)` lazily reduces `fa` from right-to-left
 *  - `reduceTo(fa)(f)(g)` eagerly reduces with an additional mapping function
 */
@typeclass trait Reducible[F[_]] extends Foldable[F] { self =>

  /**
   * Left associative reduction on 'F' using the function 'f'.
   *
   * Implementations should override this method when possible.
   */
  def reduceLeft[A](fa: F[A])(f: (A, A) => A): A

  /**
   * Right associative reduction on 'F' using the function 'f'.
   */
  def reduceRight[A](fa: F[A])(f: A => Fold[A]): Lazy[A]

  /**
   * Apply f to each element of F and combine them using the Semigroup[B].
   */
  def reduce[A](fa: F[A])(implicit A: Semigroup[A]): A =
    reduceLeft(fa)(A.combine)

  /**
   * Fold up F using the SemigroupK instance for G. Like fold, but the value is of kind * -> *.
   */
  def reduceK[G[_], A](fga: F[G[A]])(implicit G: SemigroupK[G]): G[A] =
    reduce(fga)(G.algebra)

  /**
   * Apply f to each element of fa and combine them using the given
   * Semigroup[B].
   */
  def reduceMap[A, B](fa: F[A])(f: A => B)(implicit B: Semigroup[B]): B =
    reduceTo(fa)(f)(B.combine)

  /**
   * Apply f to each element of fa and combine them with the given
   * associative function g.
   */
  def reduceTo[A, B](fa: F[A])(f: A => B)(g: (B, B) => B): B

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
    G.map(reduceTo(fa)(f)((x, y) => G.map2(x, y)((_, b) => b)))(_ => ())

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

object Reducible {
  case class EmptyReduceError() extends IllegalArgumentException("empty reduce")

  /**
   * Given a `Foldable[F]` instance, create a `Reducible[F]` instance
   * that will work for `F[X]` values that are non-empty.
   *
   * This method is unsafe -- if it was used with an empty `F[X]`
   * value an exception would be thrown.
   */
  def fromFoldableUnsafe[F[_]: Foldable]: Reducible[F] =
    new UnsafeFoldable[F]
}

/**
 * Methods that apply to 2 nested Reducible instances
 */
trait CompositeReducible[F[_], G[_]] extends Reducible[λ[α => F[G[α]]]] with CompositeFoldable[F, G] {
  implicit def F: Reducible[F]
  implicit def G: Reducible[G]

  override def reduceLeft[A](fga: F[G[A]])(f: (A, A) => A): A =
    F.reduceTo(fga)(ga => G.reduceLeft(ga)(f))(f)

  override def reduceRight[A](fga: F[G[A]])(f: A => Fold[A]): Lazy[A] =
    Lazy(foldRight(fga, Lazy(Option.empty[A])) { a =>
      f(a) match {
        case Return(a) => Return(Some(a))
        case Continue(f) => Continue(_.map(f))
        case _ => Pass
      }
    }.value.getOrElse(throw EmptyReduceError()))

  override def reduceTo[A, B](fga: F[G[A]])(f: A => B)(g: (B, B) => B): B =
    F.reduceTo(fga)(ga => G.reduceTo(ga)(f)(g))(g)
}

class UnsafeFoldable[F[_]](implicit F: Foldable[F]) extends Reducible[F] {
  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
    F.foldLeft(fa, b)(f)

  def partialFold[A, B](fa: F[A])(f: A => Fold[B]): Fold[B] =
    F.partialFold(fa)(f)

  def reduceLeft[A](fa: F[A])(f: (A, A) => A): A =
    F.foldLeft(fa, Option.empty[A]) {
      case (None, a2) => Some(a2)
      case (Some(a1), a2) => Some(f(a1, a2))
    }.getOrElse(throw EmptyReduceError())

  def reduceRight[A](fa: F[A])(f: A => Fold[A]): Lazy[A] =
    Lazy(F.foldRight(fa, Lazy(Option.empty[A])) { a =>
      f(a) match {
        case Return(a) => Return(Some(a))
        case Continue(f) => Continue(_.map(f))
        case _ => Pass
      }
    }.value.getOrElse(throw EmptyReduceError()))

  def reduceTo[A, B](fa: F[A])(f: A => B)(g: (B, B) => B): B =
    F.foldLeft(fa, Option.empty[B]) {
      case (Some(b), a) => Some(g(b, f(a)))
      case (None, a) => Some(f(a))
    }.getOrElse(throw EmptyReduceError())
}

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

  override def reduceLeft[A](fa: F[A])(f: (A, A) => A): A = {
    val (a, ga) = split(fa)
    G.foldLeft(ga, a)(f)
  }

  override def reduceRight[A](fa: F[A])(f: A => Fold[A]): Lazy[A] =
    Lazy {
      val (a, ga) = split(fa)
      G.foldRight(ga, Lazy.eager(a))(f).value
    }

  override def reduceTo[A, B](fa: F[A])(f: A => B)(g: (B, B) => B): B = {
    val (a, ga) = split(fa)
    G.foldLeft(ga, f(a))((b, a) => g(b, f(a)))
  }
}
