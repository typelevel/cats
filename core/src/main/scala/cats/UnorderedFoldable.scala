package cats

import cats.kernel.CommutativeMonoid
import simulacrum.typeclass
import cats.instances.long._

/**
 * `UnorderedFoldable` is like a `Foldable` for unordered containers.
 */
@typeclass trait UnorderedFoldable[F[_]] {

  def unorderedFoldMap[A, B: CommutativeMonoid](fa: F[A])(f: A => B): B

  def unorderedFold[A: CommutativeMonoid](fa: F[A]): A =
    unorderedFoldMap(fa)(identity)

  /**
   * Returns true if there are no elements. Otherwise false.
   */
  def isEmpty[A](fa: F[A]): Boolean =
    !nonEmpty(fa)

  def nonEmpty[A](fa: F[A]): Boolean =
    exists(fa)(Function.const(true))

  /**
   * Check whether at least one element satisfies the predicate.
   *
   * If there are no elements, the result is `false`.
   */
  def exists[A](fa: F[A])(p: A => Boolean): Boolean =
    unorderedFoldMap(fa)(a => Eval.later(p(a)))(UnorderedFoldable.orEvalMonoid).value

  /**
   * Check whether all elements satisfy the predicate.
   *
   * If there are no elements, the result is `true`.
   */
  def forall[A](fa: F[A])(p: A => Boolean): Boolean =
    unorderedFoldMap(fa)(a => Eval.later(p(a)))(UnorderedFoldable.andEvalMonoid).value

  /**
   * Find the minimum `A` item in this structure according to the `Order[A]`.
   *
   * @return `None` if the structure is empty, otherwise the minimum element
   * wrapped in a `Some`.
   *
   * @see [[Reducible#minimum]] for a version that doesn't need to return an
   * `Option` for structures that are guaranteed to be non-empty.
   *
   * @see [[maximumOption]] for maximum instead of minimum.
   */
  def minimumOption[A](fa: F[A])(implicit A: Order[A]): Option[A] =
    unorderedFoldMap[A, Option[A]](fa)(Some(_))(UnorderedFoldable.minMonoid[A])

  /**
   * Find the maximum `A` item in this structure according to the `Order[A]`.
   *
   * @return `None` if the structure is empty, otherwise the maximum element
   * wrapped in a `Some`.
   *
   * @see [[Reducible#maximum]] for a version that doesn't need to return an
   * `Option` for structures that are guaranteed to be non-empty.
   *
   * @see [[minimumOption]] for minimum instead of maximum.
   */
  def maximumOption[A](fa: F[A])(implicit A: Order[A]): Option[A] =
    unorderedFoldMap[A, Option[A]](fa)(Some(_))(UnorderedFoldable.maxMonoid[A])

  /**
   * Find the minimum `A` item in this structure according to an `Order.by(f)`.
   *
   * @return `None` if the structure is empty, otherwise the minimum element
   * wrapped in a `Some`.
   *
   * @see [[Reducible#minimumBy]] for a version that doesn't need to return an
   * `Option` for structures that are guaranteed to be non-empty.
   *
   * @see [[maximumByOption]] for maximum instead of minimum.
   */
  def minimumByOption[A, B: Order](fa: F[A])(f: A => B): Option[A] =
    minimumOption(fa)(Order.by(f))

  /**
   * Find the maximum `A` item in this structure according to an `Order.by(f)`.
   *
   * @return `None` if the structure is empty, otherwise the maximum element
   * wrapped in a `Some`.
   *
   * @see [[Reducible#maximumBy]] for a version that doesn't need to return an
   * `Option` for structures that are guaranteed to be non-empty.
   *
   * @see [[minimumByOption]] for minimum instead of maximum.
   */
  def maximumByOption[A, B: Order](fa: F[A])(f: A => B): Option[A] =
    maximumOption(fa)(Order.by(f))

  /**
   * The size of this UnorderedFoldable.
   *
   * This is overridden in structures that have more efficient size implementations
   * (e.g. Vector, Set, Map).
   *
   * Note: will not terminate for infinite-sized collections.
   */
  def size[A](fa: F[A]): Long = unorderedFoldMap(fa)(_ => 1L)
}

object UnorderedFoldable {
  private val orEvalMonoid: CommutativeMonoid[Eval[Boolean]] = new CommutativeMonoid[Eval[Boolean]] {
    val empty: Eval[Boolean] = Eval.False

    def combine(lx: Eval[Boolean], ly: Eval[Boolean]): Eval[Boolean] =
      lx.flatMap {
        case true  => Eval.True
        case false => ly
      }
  }

  private val andEvalMonoid: CommutativeMonoid[Eval[Boolean]] = new CommutativeMonoid[Eval[Boolean]] {
    val empty: Eval[Boolean] = Eval.True

    def combine(lx: Eval[Boolean], ly: Eval[Boolean]): Eval[Boolean] =
      lx.flatMap {
        case true  => ly
        case false => Eval.False
      }
  }

  private def minMonoid[A](implicit order: Order[A]): CommutativeMonoid[Option[A]] = new CommutativeMonoid[Option[A]] {

    override def empty: Option[A] =
      None

    override def combine(x: Option[A], y: Option[A]): Option[A] =
      (x, y) match {
        case (Some(x1), Some(y1)) => Some(order.min(x1, y1))
        case (None, Some(y1))     => Some(y1)
        case (Some(x1), _)        => Some(x1)
        case _                    => None
      }
  }

  private def maxMonoid[A](implicit order: Order[A]): CommutativeMonoid[Option[A]] = new CommutativeMonoid[Option[A]] {

    override def empty: Option[A] =
      None

    override def combine(x: Option[A], y: Option[A]): Option[A] =
      (x, y) match {
        case (Some(x1), Some(y1)) => Some(order.max(x1, y1))
        case (None, Some(y1))     => Some(y1)
        case (Some(x1), _)        => Some(x1)
        case _                    => None
      }
  }
}
