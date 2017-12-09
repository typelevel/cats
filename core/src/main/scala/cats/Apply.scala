package cats

import simulacrum.typeclass

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
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val someF: Option[Int => Long] = Some(_.toLong + 1L)
   * scala> val noneF: Option[Int => Long] = None
   * scala> val someInt: Option[Int] = Some(3)
   * scala> val noneInt: Option[Int] = None
   *
   * scala> Apply[Option].ap(someF)(someInt)
   * res0: Option[Long] = Some(4)
   *
   * scala> Apply[Option].ap(noneF)(someInt)
   * res1: Option[Long] = None
   *
   * scala> Apply[Option].ap(someF)(noneInt)
   * res2: Option[Long] = None
   *
   * scala> Apply[Option].ap(noneF)(noneInt)
   * res3: Option[Long] = None
   * }}}
   */
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    ap(map(fa)(a => (b: B) => (a, b)))(fb)

  /**
   * Compose two actions, discarding any value produced by the first.
   *
   * @see [[forEffect]] to discard the value of the second instead.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.data.Validated
   * scala> import Validated.{Valid, Invalid}
   *
   * scala> type ErrOr[A] = Validated[String, A]
   *
   * scala> val validInt: ErrOr[Int] = Valid(3)
   * scala> val validBool: ErrOr[Boolean] = Valid(true)
   * scala> val invalidInt: ErrOr[Int] = Invalid("Invalid int.")
   * scala> val invalidBool: ErrOr[Boolean] = Invalid("Invalid boolean.")
   *
   * scala> Apply[ErrOr].followedBy(validInt)(validBool)
   * res0: ErrOr[Boolean] = Valid(true)
   *
   * scala> Apply[ErrOr].followedBy(invalidInt)(validBool)
   * res1: ErrOr[Boolean] = Invalid(Invalid int.)
   *
   * scala> Apply[ErrOr].followedBy(validInt)(invalidBool)
   * res2: ErrOr[Boolean] = Invalid(Invalid boolean.)
   *
   * scala> Apply[ErrOr].followedBy(invalidInt)(invalidBool)
   * res3: ErrOr[Boolean] = Invalid(Invalid int.Invalid boolean.)
   * }}}
   *
   */
  def followedBy[A, B](fa: F[A])(fb: F[B]): F[B] =
    map2(fa, fb)((_, b) => b)

  /** Alias for [[followedBy]]. */
  @inline final def *>[A, B](fa: F[A])(fb: F[B]): F[B] =
    followedBy(fa)(fb)

  /**
   * Compose two actions, discarding any value produced by the second.
   *
   * @see [[followedBy]] to discard the value of the first instead.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.data.Validated
   * scala> import Validated.{Valid, Invalid}
   *
   * scala> type ErrOr[A] = Validated[String, A]
   *
   * scala> val validInt: ErrOr[Int] = Valid(3)
   * scala> val validBool: ErrOr[Boolean] = Valid(true)
   * scala> val invalidInt: ErrOr[Int] = Invalid("Invalid int.")
   * scala> val invalidBool: ErrOr[Boolean] = Invalid("Invalid boolean.")
   *
   * scala> Apply[ErrOr].forEffect(validInt)(validBool)
   * res0: ErrOr[Int] = Valid(3)
   *
   * scala> Apply[ErrOr].forEffect(invalidInt)(validBool)
   * res1: ErrOr[Int] = Invalid(Invalid int.)
   *
   * scala> Apply[ErrOr].forEffect(validInt)(invalidBool)
   * res2: ErrOr[Int] = Invalid(Invalid boolean.)
   *
   * scala> Apply[ErrOr].forEffect(invalidInt)(invalidBool)
   * res3: ErrOr[Int] = Invalid(Invalid int.Invalid boolean.)
   * }}}
   */
  def forEffect[A, B](fa: F[A])(fb: F[B]): F[A] =
    map2(fa, fb)((a, _) => a)

  /** Alias for [[forEffect]]. */
  @inline final def <*[A, B](fa: F[A])(fb: F[B]): F[A] =
    forEffect(fa)(fb)

  /**
   * ap2 is a binary version of ap, defined in terms of ap.
   */
  def ap2[A, B, Z](ff: F[(A, B) => Z])(fa: F[A], fb: F[B]): F[Z] =
    map(product(fa, product(fb, ff))) { case (a, (b, f)) => f(a, b) }

  /**
   * Applies the pure (binary) function f to the effectful values fa and fb.
   *
   * map2 can be seen as a binary version of [[cats.Functor]]#map.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val someInt: Option[Int] = Some(3)
   * scala> val noneInt: Option[Int] = None
   * scala> val someLong: Option[Long] = Some(4L)
   * scala> val noneLong: Option[Long] = None
   *
   * scala> Apply[Option].map2(someInt, someLong)((i, l) => i.toString + l.toString)
   * res0: Option[String] = Some(34)
   *
   * scala> Apply[Option].map2(someInt, noneLong)((i, l) => i.toString + l.toString)
   * res0: Option[String] = None
   *
   * scala> Apply[Option].map2(noneInt, noneLong)((i, l) => i.toString + l.toString)
   * res0: Option[String] = None
   *
   * scala> Apply[Option].map2(noneInt, someLong)((i, l) => i.toString + l.toString)
   * res0: Option[String] = None
   * }}}
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

  /**
   * Compose an `Apply[F]` and an `Apply[G]` into an `Apply[λ[α => F[G[α]]]]`.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val alo = Apply[List].compose[Option]
   *
   * scala> alo.product(List(None, Some(true), Some(false)), List(Some(2), None))
   * res1: List[Option[(Boolean, Int)]] = List(None, None, Some((true,2)), None, Some((false,2)), None)
   * }}}
   */
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
