package cats
package data

import cats.kernel._
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

/** A newtype over a `SortedSet` the construction of which is guaranteed to
  * only use an [[Order]] instance, rather than `Ordering`. The intent here is
  * that an `OrderedSet` will always use the coherent [[Order]] for a given
  * type, implying for all `OrderedSet[A]` they will have the same
  * ordering. If you bypass this restriction then your `OrderedSet` will be
  * unlawful for many of the cats typeclasses.
  *
  * This differs from `SortedSet` in that any given instance of a `SortedSet`
  * may be using any lawful, but not necessarily coherent, `Ordering`.
  */
private[data] object OrderedSetImpl extends OrderedSetInstances with Newtype {
  private[data] def create[A](s: SortedSet[A]): Type[A] =
    s.asInstanceOf[Type[A]]

  private[data] def unwrap[A](s: Type[A]): SortedSet[A] =
    s.asInstanceOf[SortedSet[A]]

  def empty[A](implicit A: Order[A]): OrderedSet[A] =
    create(SortedSet.empty(A.toOrdering))

  def from[G[_], A](value: G[A])(implicit G: Foldable[G], A: Order[A]): OrderedSet[A] =
    G.foldLeft(value, empty[A])(_.add(_))

  def of[A: Order](x: A, xs: A*): OrderedSet[A] =
    from(x +: xs)

  def apply[A: Order](x: A, xs: A*): OrderedSet[A] =
    from(x +: xs)

  def one[A: Order](x: A): OrderedSet[A] =
    of(x)

  implicit def catsSetOpsForOrderedSet[A](value: OrderedSet[A]): OrderedSetOps[A] =
    new OrderedSetOps[A](value)
}

final class OrderedSetOps[A](override val set: OrderedSet[A]) extends SetOpsForOrderedSets[OrderedSet, OrderedSet, A] {

  def toSortedSet: SortedSet[A] =
    OrderedSetImpl.unwrap(set)

  def nonEmpty: Boolean =
    toSortedSet.nonEmpty

  def isEmpty: Boolean =
    !nonEmpty

  def headOption: Option[A] =
    toSortedSet.headOption

  def lastOption: Option[A] =
    toSortedSet.lastOption

  def max: Option[A] =
    lastOption

  def min: Option[A] =
    headOption

  def minBy[B: Order](f: A => B): Option[A] =
    if (toSortedSet.isEmpty) {
      None
    } else {
      implicit val ordering: Ordering[B] = Order[B].toOrdering
      Some(toSortedSet.minBy(f))
    }

  def maxBy[B: Order](f: A => B): Option[A] =
    if (toSortedSet.isEmpty) {
      None
    } else {
      implicit val ordering: Ordering[B] = Order[B].toOrdering
      Some(toSortedSet.maxBy(f))
    }

  override def show(implicit A: Show[A]): String =
    toSortedSet.iterator.map(A.show).mkString("OrderedSet(", ", ", ")")

  override def ===(that: OrderedSet[A])(implicit A: Eq[A]): Boolean =
    Eq[Int].eqv(length, that.length) && cats.kernel.instances.StaticMethods
      .iteratorEq(toSortedSet.iterator, that.toSortedSet.iterator)

  override def add(x: A): OrderedSet[A] =
    OrderedSetImpl.create(toSortedSet + x)

  override def remove(x: A): OrderedSet[A] =
    OrderedSetImpl.create(toSortedSet - x)

  override def union(xs: OrderedSet[A]): OrderedSet[A] =
    OrderedSetImpl.create(set.toSortedSet ++ xs.toSortedSet)

  override def apply(a: A): Boolean =
    toSortedSet(a)

  override def diff(xs: OrderedSet[A]): OrderedSet[A] =
    OrderedSetImpl.create(toSortedSet -- xs.toSortedSet)

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

  override def filter(p: A => Boolean): OrderedSet[A] =
    OrderedSetImpl.create(toSortedSet.filter(p))

  override def intersect(xs: OrderedSet[A]): OrderedSet[A] =
    OrderedSetImpl.create(toSortedSet.filter(xs.contains))

  override def map[B](f: A => B)(implicit B: Order[B]): OrderedSet[B] =
    OrderedSetImpl.create(toSortedSet.map(f)(B.toOrdering))

  override def concatMap[B](f: A => OrderedSet[B])(implicit B: Order[B]): OrderedSet[B] =
    OrderedSetImpl.create(toSortedSet.flatMap(a => f(a).toSortedSet)(B.toOrdering))

  override def collect[B](pf: PartialFunction[A, B])(implicit B: Order[B]): OrderedSet[B] =
    foldLeft(OrderedSet.empty[B]) { case (acc, value) =>
      pf.lift(value).fold(acc)(value => acc.add(value))
    }

  override def zipWith[B, C](b: OrderedSet[B])(f: (A, B) => C)(implicit C: Order[C]): OrderedSet[C] = {
    implicit val ordering: Ordering[C] = C.toOrdering
    OrderedSetImpl.create(toSortedSet.lazyZip(b.toSortedSet).map(f))
  }

  override def zipWithIndex(implicit A: Order[A]): OrderedSet[(A, Int)] = {
    implicit val ordering: Ordering[A] = A.toOrdering
    OrderedSetImpl.create(cats.compat.SortedSet.zipWithIndex(toSortedSet))
  }
}

sealed abstract private[data] class OrderedSetInstances {

  implicit val catsStdInstancesForOrderedSet: Foldable[OrderedSet] with SemigroupK[OrderedSet] =
    new Foldable[OrderedSet] with SemigroupK[OrderedSet] {

      def combineK[A](x: OrderedSet[A], y: OrderedSet[A]): OrderedSet[A] = x | y

      def foldLeft[A, B](fa: OrderedSet[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: OrderedSet[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa.toSortedSet, lb)(f)

      override def foldMap[A, B](fa: OrderedSet[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.toSortedSet.iterator.map(f))

      override def get[A](fa: OrderedSet[A])(idx: Long): Option[A] = {
        @tailrec
        def go(idx: Int, it: Iterator[A]): Option[A] =
          if (it.hasNext) {
            if (idx == 0) Some(it.next())
            else {
              it.next()
              go(idx - 1, it)
            }
          } else None
        if (idx < Int.MaxValue && idx >= 0L) go(idx.toInt, fa.toSortedSet.iterator) else None
      }

      override def size[A](fa: OrderedSet[A]): Long = fa.length.toLong

      override def exists[A](fa: OrderedSet[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: OrderedSet[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def isEmpty[A](fa: OrderedSet[A]): Boolean = fa.isEmpty

      override def fold[A](fa: OrderedSet[A])(implicit A: Monoid[A]): A = A.combineAll(fa.toSortedSet)

      override def toList[A](fa: OrderedSet[A]): List[A] = fa.toSortedSet.toList

      override def toIterable[A](fa: OrderedSet[A]): Iterable[A] = fa.toSortedSet

      override def reduceLeftOption[A](fa: OrderedSet[A])(f: (A, A) => A): Option[A] =
        fa.toSortedSet.reduceLeftOption(f)

      override def find[A](fa: OrderedSet[A])(f: A => Boolean): Option[A] = fa.toSortedSet.find(f)

      override def collectFirst[A, B](fa: OrderedSet[A])(pf: PartialFunction[A, B]): Option[B] =
        fa.toSortedSet.collectFirst(pf)

      override def collectFirstSome[A, B](fa: OrderedSet[A])(f: A => Option[B]): Option[B] =
        collectFirst(fa)(Function.unlift(f))
    }

  implicit def catsStdShowForOrderedSet[A: Show]: Show[OrderedSet[A]] =
    Show.show(_.show)

  implicit def catsStdHashAndOrderForOrderedSet[A: Order]: Hash[OrderedSet[A]] with Order[OrderedSet[A]] =
    new Hash[OrderedSet[A]] with Order[OrderedSet[A]] {
      override def hash(x: OrderedSet[A]): Int =
        x.hashCode

      override def compare(x: OrderedSet[A], y: OrderedSet[A]): Int =
        Order[Int].compare(x.length, y.length) match {
          case 0 =>
            cats.kernel.instances.StaticMethods.iteratorCompare(
              x.toSortedSet.iterator,
              y.toSortedSet.iterator
            )
          case otherwise =>
            otherwise
        }
    }

  implicit def catsStdBoundedSemilatticeForOrderedSet[A: Order]: BoundedSemilattice[OrderedSet[A]] =
    new BoundedSemilattice[OrderedSet[A]] {
      override def empty: OrderedSet[A] = OrderedSet.empty[A]
      override def combine(x: OrderedSet[A], y: OrderedSet[A]): OrderedSet[A] = x | y
    }

  implicit val catsStdSemigroupalForOrderedSet: Semigroupal[OrderedSet] = new Semigroupal[OrderedSet] {
    override def product[A, B](fa: OrderedSet[A], fb: OrderedSet[B]): OrderedSet[(A, B)] = {
      val fa2: SortedSet[A] = fa.toSortedSet
      val fb2: SortedSet[B] = fb.toSortedSet
      implicit val orderingA: Ordering[A] = fa.toSortedSet.ordering
      implicit val orderingB: Ordering[B] = fb.toSortedSet.ordering
      implicit val orderAB: Order[(A, B)] = Order.fromOrdering(Ordering[(A, B)])

      OrderedSetImpl.create(fa2.flatMap(a => fb2.map(b => a -> b)))
    }
  }
}
