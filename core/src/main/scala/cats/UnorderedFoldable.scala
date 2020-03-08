package cats

import cats.instances.long._
import cats.kernel.CommutativeMonoid
import simulacrum.{noop, typeclass}

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
}

object UnorderedFoldable extends cats.instances.NTupleUnorderedFoldableInstances {
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
}
