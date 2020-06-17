package cats
package data

import java.io.Serializable
import cats.arrow.{ArrowChoice, CommutativeArrow}

/**
 * A function type of a single input that can do function composition
 * (via `andThen` and `compose`) in constant stack space with amortized
 * linear time application (in the number of constituent functions).
 *
 * Example:
 *
 * {{{
 *   val seed = AndThen((x: Int) => x + 1)
 *   val f = (0 until 10000).foldLeft(seed)((acc, _) => acc.andThen(_ + 1))
 *
 *   // This should not trigger stack overflow ;-)
 *   f(0)
 * }}}
 *
 * This can be used to build stack safe data structures that make
 * use of lambdas. The perfect candidates for usage with `AndThen`
 * are the data structures using a signature like this (where
 * `F[_]` is a monadic type):
 *
 * {{{
 *   A => F[B]
 * }}}
 *
 * As an example, if we described this data structure, the naive
 * solution for that `map` is stack unsafe:
 *
 * {{{
 *   case class Resource[F[_], A, B](
 *     acquire: F[A],
 *     use: A => F[B],
 *     release: A => F[Unit]) {
 *
 *     def flatMap[C](f: B => C)(implicit F: Functor[F]): Resource[F, A, C] = {
 *       Resource(
 *         ra.acquire,
 *         // Stack Unsafe!
 *         a => ra.use(a).map(f),
 *         ra.release)
 *     }
 *   }
 * }}}
 *
 * To describe a `flatMap` operation for this data type, `AndThen`
 * can save the day:
 *
 * {{{
 *   def flatMap[C](f: B => C)(implicit F: Functor[F]): Resource[F, A, C] = {
 *     Resource(
 *       ra.acquire,
 *       AndThen(ra.use).andThen(_.map(f)),
 *       ra.release)
 *   }
 * }}}
 */
sealed abstract class AndThen[-T, +R] extends (T => R) with Product with Serializable {

  import AndThen._

  final def apply(a: T): R =
    runLoop(a)

  override def andThen[A](g: R => A): AndThen[T, A] =
    // Fusing calls up to a certain threshold, using the fusion
    // technique implemented for `cats.effect.IO#map`
    this match {
      case Single(f, index) if index != fusionMaxStackDepth =>
        Single(f.andThen(g), index + 1)
      case _ =>
        andThenF(AndThen(g))
    }

  override def compose[A](g: A => T): AndThen[A, R] =
    // Fusing calls up to a certain threshold, using the fusion
    // technique implemented for `cats.effect.IO#map`
    this match {
      case Single(f, index) if index != fusionMaxStackDepth =>
        Single(f.compose(g), index + 1)
      case _ =>
        composeF(AndThen(g))
    }

  private def runLoop(start: T): R = {
    var self: AndThen[Any, Any] = this.asInstanceOf[AndThen[Any, Any]]
    var current: Any = start.asInstanceOf[Any]
    var continue = true

    while (continue) {
      self match {
        case Single(f, _) =>
          current = f(current)
          continue = false

        case Concat(Single(f, _), right) =>
          current = f(current)
          self = right.asInstanceOf[AndThen[Any, Any]]

        case Concat(left @ Concat(_, _), right) =>
          self = left.rotateAccum(right)
      }
    }
    current.asInstanceOf[R]
  }

  final private def andThenF[X](right: AndThen[R, X]): AndThen[T, X] =
    Concat(this, right)
  final private def composeF[X](right: AndThen[X, T]): AndThen[X, R] =
    Concat(right, this)

  // converts left-leaning to right-leaning
  final protected def rotateAccum[E](_right: AndThen[R, E]): AndThen[T, E] = {
    var self: AndThen[Any, Any] = this.asInstanceOf[AndThen[Any, Any]]
    var right: AndThen[Any, Any] = _right.asInstanceOf[AndThen[Any, Any]]
    var continue = true
    while (continue) {
      self match {
        case Concat(left, inner) =>
          self = left.asInstanceOf[AndThen[Any, Any]]
          right = inner.asInstanceOf[AndThen[Any, Any]].andThenF(right)

        case _ => // Single
          self = self.andThenF(right)
          continue = false
      }
    }
    self.asInstanceOf[AndThen[T, E]]
  }

  override def toString: String =
    "AndThen$" + System.identityHashCode(this)
}

object AndThen extends AndThenInstances0 {

  /**
   * Builds an [[AndThen]] reference by wrapping a plain function. */
  def apply[A, B](f: A => B): AndThen[A, B] =
    f match {
      case ref: AndThen[A, B] @unchecked => ref
      case _                             => Single(f, 0)
    }

  final private case class Single[-A, +B](f: A => B, index: Int) extends AndThen[A, B]
  final private case class Concat[-A, E, +B](left: AndThen[A, E], right: AndThen[E, B]) extends AndThen[A, B]

  /**
   * Establishes the maximum stack depth when fusing `andThen` or
   * `compose` calls.
   *
   * The default is `128`, from which we subtract one as an optimization,
   * a "!=" comparison being slightly more efficient than a "<".
   *
   * This value was reached by taking into account the default stack
   * size as set on 32 bits or 64 bits, Linux or Windows systems,
   * being enough to notice performance gains, but not big enough
   * to be in danger of triggering a stack-overflow error.
   */
  final private val fusionMaxStackDepth = 127
}

abstract private[data] class AndThenInstances0 extends AndThenInstances1 {

  /**
   * [[cats.Monad]] instance for [[AndThen]].
   */
  implicit def catsDataMonadForAndThen[T]: Monad[AndThen[T, *]] =
    new Monad[AndThen[T, *]] {
      // Piggybacking on the instance for Function1
      private[this] val fn1 = instances.all.catsStdMonadForFunction1[T]

      def pure[A](x: A): AndThen[T, A] =
        AndThen(fn1.pure[A](x))

      def flatMap[A, B](fa: AndThen[T, A])(f: A => AndThen[T, B]): AndThen[T, B] =
        AndThen(fn1.flatMap(fa)(f))

      override def map[A, B](fa: AndThen[T, A])(f: A => B): AndThen[T, B] =
        AndThen(f).compose(fa)

      def tailRecM[A, B](a: A)(f: A => AndThen[T, Either[A, B]]): AndThen[T, B] =
        AndThen(fn1.tailRecM(a)(f))
    }

  /**
   * [[cats.ContravariantMonoidal]] instance for [[AndThen]].
   */
  implicit def catsDataContravariantMonoidalForAndThen[R: Monoid]: ContravariantMonoidal[AndThen[*, R]] =
    new ContravariantMonoidal[AndThen[*, R]] {
      // Piggybacking on the instance for Function1
      private[this] val fn1 = instances.all.catsStdContravariantMonoidalForFunction1[R]

      def unit: AndThen[Unit, R] =
        AndThen(fn1.unit)

      def contramap[A, B](fa: AndThen[A, R])(f: B => A): AndThen[B, R] =
        fa.compose(f)

      def product[A, B](fa: AndThen[A, R], fb: AndThen[B, R]): AndThen[(A, B), R] =
        AndThen(fn1.product(fa, fb))
    }

  /**
   * [[cats.arrow.ArrowChoice ArrowChoice]] and
   * [[cats.arrow.CommutativeArrow CommutativeArrow]] instances
   * for [[AndThen]].
   */
  implicit val catsDataArrowForAndThen: ArrowChoice[AndThen] with CommutativeArrow[AndThen] =
    new ArrowChoice[AndThen] with CommutativeArrow[AndThen] {
      // Piggybacking on the instance for Function1
      private[this] val fn1 = instances.all.catsStdInstancesForFunction1

      def choose[A, B, C, D](f: AndThen[A, C])(g: AndThen[B, D]): AndThen[Either[A, B], Either[C, D]] =
        AndThen(fn1.choose(f)(g))

      def lift[A, B](f: A => B): AndThen[A, B] =
        AndThen(f)

      def first[A, B, C](fa: AndThen[A, B]): AndThen[(A, C), (B, C)] =
        AndThen(fn1.first(fa))

      override def split[A, B, C, D](f: AndThen[A, B], g: AndThen[C, D]): AndThen[(A, C), (B, D)] =
        AndThen(fn1.split(f, g))

      def compose[A, B, C](f: AndThen[B, C], g: AndThen[A, B]): AndThen[A, C] =
        f.compose(g)
    }
}

abstract private[data] class AndThenInstances1 {

  /**
   * [[cats.Contravariant]] instance for [[AndThen]].
   */
  implicit def catsDataContravariantForAndThen[R]: Contravariant[AndThen[*, R]] =
    new Contravariant[AndThen[*, R]] {
      def contramap[T1, T0](fa: AndThen[T1, R])(f: T0 => T1): AndThen[T0, R] =
        fa.compose(f)
    }
}
