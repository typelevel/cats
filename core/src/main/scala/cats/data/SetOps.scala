package cats
package data

import scala.collection.immutable.SortedSet

/** A set of ops methods which should be implemented for all set like
  * structures, both the normal and non-empty variants.
  *
  * The purpose of this trait is merely to help ensure consistent
  * implementation between the different concrete types.
  *
  * @tparam F This is the input type we are operating on. It is the result
  *           type for operations which can never yield an empty set.
  * @tparam G This is the the result type for operations which ''may'' yield
  *           an empty set.
  *
  * The use of the types `F` and `G` allow us to share this trait between set
  * types which may be have an empty state, and those which many not,
  * e.g. `OrderedSet` and `NonEmptySet`.
  */
private[data] trait SetOps[F[_], G[_], A] {

  // Abstract

  def set: F[A]

  def add(a: A): F[A]

  def remove(a: A): G[A]

  def apply(a: A): Boolean

  def diff(as: F[A]): G[A]

  def union(as: F[A]): F[A]

  def length: Int

  def foldLeft[B](b: B)(f: (B, A) => B): B

  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]

  def exists(f: A => Boolean): Boolean

  def forall(f: A => Boolean): Boolean

  def show(implicit A: Show[A]): String

  def ===(that: F[A])(implicit A: Eq[A]): Boolean

  def find(f: A => Boolean): Option[A]

  def intersect(as: F[A]): G[A]

  def filter(p: A => Boolean): G[A]

  def nonEmpty: Boolean

  // NonAbstract

  def +(a: A): F[A] = add(a)

  def -(a: A): G[A] = remove(a)

  def contains(a: A): Boolean = apply(a)

  def ++(as: F[A]): F[A] = union(as)

  def |(as: F[A]): F[A] = union(as)

  def --(as: F[A]): G[A] = diff(as)

  def &~(as: F[A]): G[A] = diff(as)

  def filterNot(p: A => Boolean): G[A] = filter(t => !p(t))

  def isEmpty: Boolean =
    !nonEmpty
}

/** As `SetOps`, but adds methods which require an [[Order]] constraint if the
  * underlying set like structure is ordered.
  */
private[data] trait SetOpsForOrderedSets[F[_], G[_], A] extends SetOps[F, G, A] {

  // Abstract

  def toSortedSet: SortedSet[A]

  def map[B](f: A => B)(implicit B: Order[B]): F[B]

  def concatMap[B](f: A => F[B])(implicit B: Order[B]): F[B]

  def zipWith[B, C](b: F[B])(f: (A, B) => C)(implicit C: Order[C]): F[C]

  def zipWithIndex(implicit A: Order[A]): F[(A, Int)]

  def collect[B](pf: PartialFunction[A, B])(implicit B: Order[B]): G[B]

  // Concrete From SetOps

  override def apply(a: A): Boolean =
    toSortedSet(a)

  override def nonEmpty: Boolean =
    toSortedSet.nonEmpty

  override def length: Int =
    toSortedSet.size

  override def foldLeft[B](b: B)(f: (B, A) => B): B =
    toSortedSet.foldLeft(b)(f)

  override def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    Foldable.iterateRight(toSortedSet, lb)(f)

  override def exists(f: A => Boolean): Boolean =
    toSortedSet.exists(f)

  override def forall(f: A => Boolean): Boolean =
    toSortedSet.forall(f)

  override def find(f: A => Boolean): Option[A] =
    toSortedSet.find(f)
}
