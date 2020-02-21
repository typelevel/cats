package cats

import cats.instances.long._
import cats.kernel.{CommutativeMonoid, CommutativeSemigroup}
import simulacrum.{noop, typeclass}

/**
 * `UnorderedFoldable` is like a `Foldable` for unordered containers.
 */
@typeclass trait UnorderedFoldable[F[_]] {

  def unorderedFoldMap[A, B: CommutativeMonoid](fa: F[A])(f: A => B): B

  def unorderedFold[A: CommutativeMonoid](fa: F[A]): A =
    unorderedFoldMap(fa)(identity)

  def unorderedReduceOption[A](fa: F[A])(implicit A: CommutativeSemigroup[A]): Option[A] =
    unorderedFoldMap(fa)(Some(_): Option[A])(cats.instances.option.catsKernelStdCommutativeMonoidForOption(A))

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
   * The size of this UnorderedFoldable.
   *
   * This is overridden in structures that have more efficient size implementations
   * (e.g. Vector, Set, Map).
   *
   * Note: will not terminate for infinite-sized collections.
   */
  def size[A](fa: F[A]): Long = unorderedFoldMap(fa)(_ => 1L)

  /**
   * Count the number of elements in the structure that satisfy the given predicate.
   *
   * For example:
   * {{{
   * scala> import cats.implicits._
   * scala> val map1 = Map[Int, String]()
   * scala> val p1: String => Boolean = _.length > 0
   * scala> UnorderedFoldable[Map[Int, *]].count(map1)(p1)
   * res0: Long = 0
   *
   * scala> val map2 = Map(1 -> "hello", 2 -> "world", 3 -> "!")
   * scala> val p2: String => Boolean = _.length > 1
   * scala> UnorderedFoldable[Map[Int, *]].count(map2)(p2)
   * res1: Long = 2
   * }}}
   */
  @noop
  def count[A](fa: F[A])(p: A => Boolean): Long =
    unorderedFoldMap(fa)(a => if (p(a)) 1L else 0L)

  /**
   * Find the minimum `A` item in this structure according to the `Order[A]`.
   *
   * @return `None` if the structure is empty, otherwise the minimum element
   * wrapped in a `Some`.
   *
   * @see [[maximumOption]] for maximum instead of minimum.
   */
  def minimumOption[A: Order](fa: F[A]): Option[A] =
    unorderedReduceOption(fa)(UnorderedFoldable.minCommutativeSemigroup)

  /**
   * Find the maximum `A` item in this structure according to the `Order[A]`.
   *
   * @return `None` if the structure is empty, otherwise the maximum element
   * wrapped in a `Some`.
   *
   * @see [[minimumOption]] for minimum instead of maximum.
   */
  def maximumOption[A: Order](fa: F[A]): Option[A] =
    unorderedReduceOption(fa)(UnorderedFoldable.maxCommutativeSemigroup)

  /**
   * Find the minimum `A` item in this structure according to an `Order.by(f)`.
   *
   * @return `None` if the structure is empty, otherwise the minimum element
   * wrapped in a `Some`.
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
   * @see [[minimumByOption]] for minimum instead of maximum.
   */
  def maximumByOption[A, B: Order](fa: F[A])(f: A => B): Option[A] =
    maximumOption(fa)(Order.by(f))
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

  private def minCommutativeSemigroup[A: Order]: CommutativeSemigroup[A] = new CommutativeSemigroup[A] {
    override def combine(x: A, y: A): A = Order.min(x, y)
  }

  private def maxCommutativeSemigroup[A: Order]: CommutativeSemigroup[A] = new CommutativeSemigroup[A] {
    override def combine(x: A, y: A): A = Order.max(x, y)
  }
}
