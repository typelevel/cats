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
    unorderedFoldMap(fa)(a => Eval.later(p(a)))(UnorderedFoldable.commutativeMonoidEval(UnorderedFoldable.orMonoid)).value

  /**
   * Check whether all elements satisfy the predicate.
   *
   * If there are no elements, the result is `true`.
   */
  def forall[A](fa: F[A])(p: A => Boolean): Boolean =
    unorderedFoldMap(fa)(a => Eval.later(p(a)))(UnorderedFoldable.commutativeMonoidEval(UnorderedFoldable.andMonoid)).value

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
  private val orMonoid: CommutativeMonoid[Boolean] = new CommutativeMonoid[Boolean] {
    val empty: Boolean = false

    def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  private val andMonoid: CommutativeMonoid[Boolean] = new CommutativeMonoid[Boolean] {
    val empty: Boolean = true

    def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  private def commutativeMonoidEval[A: CommutativeMonoid]: CommutativeMonoid[Eval[A]] =
    new EvalMonoid[A] with CommutativeMonoid[Eval[A]] { val algebra = Monoid[A] }

}
