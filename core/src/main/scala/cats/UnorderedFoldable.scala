package cats

import cats.kernel.CommutativeMonoid
import simulacrum.typeclass
import cats.instances.set._
/**
  * `UnorderedFoldable` is like a `Foldable` for unordered containers.
  */
@typeclass trait UnorderedFoldable[F[_]] {

  def unorderedFoldMap[A, B: CommutativeMonoid](fa: F[A])(f: A => B): B

  def unorderedFold[A: CommutativeMonoid](fa: F[A]): A =
    unorderedFoldMap(fa)(identity)

  def toSet[A](fa: F[A]): Set[A] =
    unorderedFoldMap(fa)(a => Set(a))

  /**
    * Returns true if there are no elements. Otherwise false.
    */
  def isEmpty[A](fa: F[A]): Boolean =
    exists(fa)(Function.const(true))

  def nonEmpty[A](fa: F[A]): Boolean =
    !isEmpty(fa)

  /**
    * Check whether at least one element satisfies the predicate.
    *
    * If there are no elements, the result is `false`.
    */
  def exists[A](fa: F[A])(p: A => Boolean): Boolean =
    unorderedFoldMap(fa)(p)(UnorderedFoldable.orMonoid)

  /**
    * Check whether all elements satisfy the predicate.
    *
    * If there are no elements, the result is `true`.
    */
  def forall[A](fa: F[A])(p: A => Boolean): Boolean =
    unorderedFoldMap(fa)(p)(UnorderedFoldable.andMonoid)

  /**
    * The size of this UnorderedFoldable.
    *
    * This is overriden in structures that have more efficient size implementations
    * (e.g. Vector, Set, Map).
    *
    * Note: will not terminate for infinite-sized collections.
    */
  def size[A](fa: F[A]): Long = unorderedFoldMap(fa)(_ => 1)
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

}
