package cats

import cats.kernel.CommutativeMonoid
import cats.kernel.instances.long._
import cats.kernel.instances.unit._
import simulacrum.typeclass

/**
  * A type class for potentially unordered data structures that can be folded.
  * to a summary value.
  *
  * In the case of a collection (such as `List` or `Vector`), these
  * methods will fold together (combine) the values contained in the
  * collection to produce a single result.
  *
  * This is a weaker version of [[Foldable]] that requires that the target
  * [[Monoid]] of the `foldMap` operation be commutative, so that the lack
  * of an ordering constraint on the data structure cannot be observed via
  * `foldMap`.
  *
  * Because the folding order is unspecified, `foldLeft` and `foldRight` are
  * not able to be implemented, and the fundamental operation is `foldMapUnordered`.
  *
  * Every [[Foldable]] is an [[UnorderedFoldable]], as every [[CommutativeMonoid]]
  * is a [[Monoid]].
  */
@typeclass trait UnorderedFoldable[F[_]] { self =>

  /**
    * Fold implemented by mapping `A` values into `B` and then
    * combining them using the given `CommutativeMonoid[B]` instance.
    *
    * As `B` is a [[CommutativeMonoid]], callers should be unable to observe
    * the possible lack of a consistent folding order.
    *
    * If this [[UnorderedFoldable]] is also a [[Foldable]], it is required
    * that this method behave identically to [[Foldable.foldMap]].
    */
  def foldMapUnordered[A, B](fa: F[A])(f: A => B)(implicit B: CommutativeMonoid[B]): B

  /**
    * Fold implemented using the given Monoid[A] instance.
    */
  def foldUnordered[A](fa: F[A])(implicit A: CommutativeMonoid[A]): A =
    foldMapUnordered(fa)(identity)

  /**
    * The size of this structure.
    *
    * This is overridden in structures that have more efficient size implementations
    * (e.g. Vector, Set, Map).
    *
    * Note: will not terminate for infinite-sized collections.
    *
    * {{{
    *   scala> import cats._, implicits._
    *   scala> val F = Foldable[List]
    *   scala> F.size(Nil)
    *   res0: Long = 0
    *   scala> F.size(1 :: 2 :: 3 :: Nil)
    *   res0: Long = 3
    * }}}
    */
  def size[A](fa: F[A]): Long = foldMapUnordered(fa)(_ => 1L)

  /**
    * Traverse `F[A]` using `CommutativeApplicative[G]`.
    *
    * `A` values will be mapped into `G[B]` and combined using
    * `Applicative#map2`.
    *
    * This method is primarily useful when `G[_]` represents an action
    * or effect, and the specific `A` aspect of `G[A]` is not otherwise
    * needed.
    */
  def traverseUnordered_[G[_], A, B](fga: F[A])(f: A => G[B])(implicit G: CommutativeApplicative[G]): G[Unit] =
    foldMapUnordered(fga)(_ => G.pure(()))(CommutativeApplicative.monoid[G, Unit])

  /**
    * Sequence `F[G[A]]` using `CommutativeApplicative[G]`.
    *
    * This is similar to `traverse_` except it operates on `F[G[A]]`
    * values, so no additional functions are needed.
    *
    */
  def sequenceUnordered_[G[_]: CommutativeApplicative, A](fga: F[G[A]]): G[Unit] =
    traverseUnordered_(fga)(identity)

  /**
    * Check whether at least one element satisfies the predicate.
    *
    * If there are no elements, the result is `false`.
    */
  def exists[A](fa: F[A])(p: A => Boolean): Boolean =
    foldMapUnordered(fa)(a => Eval.later(p(a)))(new CommutativeMonoid[Eval[Boolean]] {
      def empty = Eval.False
      def combine(x: Eval[Boolean], y: Eval[Boolean]): Eval[Boolean] =
        x.flatMap(xv => if (xv) Eval.True else y)
    }).value

  /**
    * Check whether all elements satisfy the predicate.
    *
    * If there are no elements, the result is `true`.
    */
  def forall[A](fa: F[A])(p: A => Boolean): Boolean =
    foldMapUnordered(fa)(a => Eval.later(p(a)))(new CommutativeMonoid[Eval[Boolean]] {
      def empty = Eval.True
      def combine(x: Eval[Boolean], y: Eval[Boolean]): Eval[Boolean] =
        x.flatMap(xv => if (xv) y else Eval.False)
    }).value

  /**
    * Returns true if there are no elements. Otherwise false.
    */
  def isEmpty[A](fa: F[A]): Boolean =
    forall(fa)(_ => false)

  def nonEmpty[A](fa: F[A]): Boolean =
    !isEmpty(fa)

  def findAny[A](fa: F[A])(p: A => Boolean): Option[A] =
    foldMapUnordered(fa)(a => Eval.later(Some(a).filter(p)))(new CommutativeMonoid[Eval[Option[A]]] {
      def empty: Eval[Option[A]] = Eval.now(None)
      def combine(x: Eval[Option[A]], y: Eval[Option[A]]): Eval[Option[A]] =
        x.flatMap(_.fold(y)(_ => x))
    }).value

  def compose[G[_]: UnorderedFoldable]: UnorderedFoldable[λ[α => F[G[α]]]] =
    new ComposedUnorderedFoldable[F, G] {
      val F = self
      val G = UnorderedFoldable[G]
    }
}

object UnorderedFoldable
