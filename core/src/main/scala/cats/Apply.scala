package cats

import simulacrum.{typeclass, noop}

/**
 * Weaker version of Applicative[F]; has apply but not pure.
 *
 * Must obey the laws defined in cats.laws.ApplyLaws.
 */
@typeclass(excludeParents = List("ApplyArityFunctions"))
trait Apply[F[_]] extends Functor[F] with Semigroupal[F] with ApplyArityFunctions[F] { self =>

  /**
   * Given a value and a function in the Apply context, applies the
   * function to the value.
   */
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  /** Compose two actions, discarding any value produced by the first. */
  def apR[A, B](fa: F[A])(fb: F[B]): F[B] =
    map2(fa, fb)((_, b) => b)

  /** Compose two actions, discarding any value produced by the second. */
  def apL[A, B](fa: F[A])(fb: F[B]): F[A] =
    map2(fa, fb)((a, _) => a)

  override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    ap(map(fa)(a => (b: B) => (a, b)))(fb)

  /** Alias for [[apR]]. */
  @deprecated("Use *> or apR instead.", "1.0.0-RC2")
  @noop @inline final def followedBy[A, B](fa: F[A])(fb: F[B]): F[B] =
    apR(fa)(fb)

  /** Alias for [[apL]]. */
  @deprecated("Use <* or apL instead.", "1.0.0-RC2")
  @noop @inline final def forEffect[A, B](fa: F[A])(fb: F[B]): F[A] =
    apL(fa)(fb)

  /**
   * ap2 is a binary version of ap, defined in terms of ap.
   */
  def ap2[A, B, Z](ff: F[(A, B) => Z])(fa: F[A], fb: F[B]): F[Z] =
    map(product(fa, product(fb, ff))) { case (a, (b, f)) => f(a, b) }

  /**
   * Applies the pure (binary) function f to the effectful values fa and fb.
   *
   * map2 can be seen as a binary version of [[cats.Functor]]#map.
   */
  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    map(product(fa, fb)) { case (a, b) => f(a, b) }

  /**
   * Similar to [[map2]] but uses [[Eval]] to allow for laziness in the `F[B]`
   * argument. This can allow for "short-circuiting" of computations.
   *
   * NOTE: the default implementation of `map2Eval` does not short-circuit
   * computations. For data structures that can benefit from laziness, [[Apply]]
   * instances should override this method.
   *
   * In the following example, `x.map2(bomb)(_ + _)` would result in an error,
   * but `map2Eval` "short-circuits" the computation. `x` is `None` and thus the
   * result of `bomb` doesn't even need to be evaluated in order to determine
   * that the result of `map2Eval` should be `None`.
   *
   * {{{
   * scala> import cats.{Eval, Later}
   * scala> import cats.implicits._
   * scala> val bomb: Eval[Option[Int]] = Later(sys.error("boom"))
   * scala> val x: Option[Int] = None
   * scala> x.map2Eval(bomb)(_ + _).value
   * res0: Option[Int] = None
   * }}}
   */
  def map2Eval[A, B, Z](fa: F[A], fb: Eval[F[B]])(f: (A, B) => Z): Eval[F[Z]] =
    fb.map(fb => map2(fa, fb)(f))

  def compose[G[_]: Apply]: Apply[λ[α => F[G[α]]]] =
    new ComposedApply[F, G] {
      val F = self
      val G = Apply[G]
    }
}

object Apply {
  def semigroup[F[_], A](implicit f: Apply[F], sg: Semigroup[A]): Semigroup[F[A]] =
    new ApplySemigroup[F, A](f, sg)
}

private[cats] class ApplySemigroup[F[_], A](f: Apply[F], sg: Semigroup[A]) extends Semigroup[F[A]] {
  def combine(a: F[A], b: F[A]): F[A] =
    f.map2(a, b)(sg.combine)
}
