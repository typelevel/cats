package cats

import cats.kernel.CommutativeMonoid
import simulacrum.typeclass

import scala.collection.mutable

/**
  * `UnorderedFoldable` is like a `Foldable` for unordered containers.
  */
@typeclass trait UnorderedFoldable[F[_]] {

  /**
    * Left associative fold on 'F' using the function 'f'.
    */
  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B

  def unorderedFoldMap[A, B: CommutativeMonoid](fa: F[A])(f: A => B): B =
    foldLeft(fa, Monoid[B].empty)((b, a) => Monoid[B].combine(b, f(a)))

  def unorderedFold[A: CommutativeMonoid](fa: F[A]): A =
    unorderedFoldMap(fa)(identity)

  def toSet[A](fa: F[A]): Set[A] =
    foldLeft(fa, mutable.ListBuffer.empty[A]) { (buf, a) =>
      buf += a
    }.toSet


  /**
    * Returns true if there are no elements. Otherwise false.
    */
  def isEmpty[A](fa: F[A]): Boolean =
    foldLeft(fa, true)((_, _) => false)

  def nonEmpty[A](fa: F[A]): Boolean =
    !isEmpty(fa)

  /**
    * Find the first element matching the predicate, if one exists.
    */
  def find[A](fa: F[A])(f: A => Boolean): Option[A] =
    foldLeft(fa, Option.empty[A]) { (lb, a) =>
      if (f(a)) Some(a) else lb
    }

  /**
    * Check whether at least one element satisfies the predicate.
    *
    * If there are no elements, the result is `false`.
    */
  def exists[A](fa: F[A])(p: A => Boolean): Boolean =
    foldLeft(fa, false) { (lb, a) =>
      if (p(a)) true else lb
    }

  /**
    * Check whether all elements satisfy the predicate.
    *
    * If there are no elements, the result is `true`.
    */
  def forall[A](fa: F[A])(p: A => Boolean): Boolean =
    foldLeft(fa, true) { (lb, a) =>
      if (p(a)) lb else false
    }
}
