package cats
package data

import cats.implicits._
import Chain._

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

/**
 * Trivial catenable sequence. Supports O(1) append, and (amortized)
 * O(1) `uncons`, such that walking the sequence via N successive `uncons`
 * steps takes O(N).
 */
sealed abstract class Chain[+A] {

  /** Returns the head and tail of this Chain if non empty, none otherwise. Amortized O(1). */
  final def uncons: Option[(A, Chain[A])] = {
    var c: Chain[A] = this
    val rights = new collection.mutable.ArrayBuffer[Chain[A]]
    // scalastyle:off null
    var result: Option[(A, Chain[A])] = null
    while (result eq null) {
      c match {
        case Singleton(a) =>
          val next =
            if (rights.isEmpty) nil
            else rights.reduceLeft((x, y) => Append(y, x))
          result = Some(a -> next)
        case Append(l, r) => c = l; rights += r
        case Wrap(seq) =>
          val tail = seq.tail
          val next = fromSeq(tail)
          result = Some((seq.head, next))
        case Empty =>
          if (rights.isEmpty) {
            result = None
          } else {
            c = rights.last
            rights.trimEnd(1)
          }
      }
    }
    // scalastyle:on null
    result
  }

  /** Returns true if there are no elements in this collection. */
  def isEmpty: Boolean

  /** Returns false if there are no elements in this collection. */
  def nonEmpty: Boolean = !isEmpty

  /** Concatenates this with `c` in O(1) runtime. */
  final def ++[A2 >: A](c: Chain[A2]): Chain[A2] =
    append(this, c)

  /** Returns a new Chain consisting of `a` followed by this. O(1) runtime. */
  final def cons[A2 >: A](a: A2): Chain[A2] =
    append(one(a), this)

  /** Alias for [[cons]]. */
  final def +:[A2 >: A](a: A2): Chain[A2] =
    cons(a)

  /** Returns a new Chain consisting of this followed by `a`. O(1) runtime. */
  final def snoc[A2 >: A](a: A2): Chain[A2] =
    append(this, one(a))

  /** Alias for [[snoc]]. */
  final def :+[A2 >: A](a: A2): Chain[A2] =
    snoc(a)

  /** Applies the supplied function to each element and returns a new Chain. */
  final def map[B](f: A => B): Chain[B] =
    fromSeq(iterator.map(f).toVector)

  /** Applies the supplied function to each element and returns a new Chain from the concatenated results */
  final def flatMap[B](f: A => Chain[B]): Chain[B] =
    foldLeft(nil: Chain[B])((acc, a) => acc ++ f(a))

  /** Applies the supplied function to each element and returns a new Chain from the concatenated results */
  final def flatMapIterator[B](f: A => Chain[B]): Chain[B] = {
    var result = empty[B]
    val iter = iterator
    while (iter.hasNext) { result = result ++ f(iter.next) }
    result
  }

  /** Folds over the elements from left to right using the supplied initial value and function. */
  final def foldLeft[B](z: B)(f: (B, A) => B): B = {
    var result = z
    val iter = iterator
    while (iter.hasNext) { result = f(result, iter.next) }
    result
  }

  /** Folds over the elements from right to left using the supplied initial value and function. */
  final def foldRight[B](z: B)(f: (A, B) => B): B = {
    var result = z
    val iter = reverseIterator
    while (iter.hasNext) { result = f(iter.next, result) }
    result
  }

  /** Collect `B` from this for which `f` is defined */
  final def collect[B](pf: PartialFunction[A, B]): Chain[B] =
    foldLeft(Chain.nil: Chain[B]) { (acc, a) =>
      // trick from TraversableOnce, used to avoid calling both isDefined and apply (or calling lift)
      val x = pf.applyOrElse(a, sentinel)
      if (x.asInstanceOf[AnyRef] ne sentinel) acc :+ x.asInstanceOf[B]
      else acc
    }

  /** Remove elements not matching the predicate */
  final def filter(f: A => Boolean): Chain[A] =
    collect { case a if f(a) => a }

  /** Remove elements matching the predicate */
  final def filterNot(f: A => Boolean): Chain[A] =
    filter(a => !f(a))

  /** Find the first element matching the predicate, if one exists */
  final def find(f: A => Boolean): Option[A] = {
    var result: Option[A] = Option.empty[A]
    foreachUntil { a =>
      val b = f(a)
      if (b) result = Option(a)
      b
    }
    result
  }

  final def findIterator(f: A => Boolean): Option[A] = {
    val iter = iterator
    var result: Option[A] = Option.empty[A]
    while (iter.hasNext && result.isEmpty) {
      val a = iter.next
      if (f(a)) result = Some(a)
    }
    result
  }

  /** Check whether at least one element satisfies the predicate */
  final def exists(f: A => Boolean): Boolean = {
    var result: Boolean = false
    foreachUntil { a =>
      val b = f(a)
      if (b) result = true
      b
    }
    result
  }

  /** Check whether all elements satisfy the predicate */
  final def forall(f: A => Boolean): Boolean = {
    var result: Boolean = true
    foreachUntil { a =>
      val b = f(a)
      if (!b) result = false
      !b
    }
    result
  }

  /** Check whether an element is in this structure */
  final def contains[AA >: A](a: AA)(implicit A: Eq[AA]): Boolean =
    exists(A.eqv(a, _))

  /** Zips this `Chain` with another `Chain` and applies a function for each pair of elements. */
  final def zipWith[B, C](other: Chain[B])(f: (A, B) => C): Chain[C] =
    if (this.isEmpty || other.isEmpty) Chain.Empty
    else {
      val iterA = iterator
      val iterB = other.iterator

      var result: Chain[C] = Chain.one(f(iterA.next(), iterB.next()))

      while (iterA.hasNext && iterB.hasNext) {
        result = result :+ f(iterA.next(), iterB.next())
      }
      result
    }

  /**
   * Groups elements inside this `Chain` according to the `Order`
   * of the keys produced by the given mapping function.
   */
  final def groupBy[B](f: A => B)(implicit B: Order[B]): SortedMap[B, NonEmptyChain[A]] = {
    implicit val ordering: Ordering[B] = B.toOrdering
    var m = SortedMap.empty[B, NonEmptyChain[A]]
    val iter = iterator

    while (iter.hasNext) {
      val elem = iter.next
      val k = f(elem)

      m.get(k) match {
        case None => m += ((k, NonEmptyChain.one(elem))); ()
        case Some(cat) => m = m.updated(k, cat :+ elem)
      }
    }
    m
  }

  /** Reverses this `Chain` */
  def reverse: Chain[A] =
    fromSeq(reverseIterator.toVector)


  /**
   * Yields to Some(a, Chain[A]) with `a` removed where `f` holds for the first time,
   * otherwise yields None, if `a` was not found
   * Traverses only until `a` is found.
   */
  final def deleteFirst(f: A => Boolean): Option[(A, Chain[A])] = {
    @tailrec
    def go(rem: Chain[A], acc: Chain[A]): Option[(A, Chain[A])] =
      rem.uncons match {
        case Some((a, tail)) =>
          if (!f(a)) go(tail, acc :+ a)
          else Some((a, acc ++ tail))

        case None => None
      }
    go(this, Chain.nil)
  }

  /** Applies the supplied function to each element, left to right. */
  private final def foreach(f: A => Unit): Unit = foreachUntil { a => f(a); false }

  /** Applies the supplied function to each element, left to right, but stops when true is returned */
  // scalastyle:off null return cyclomatic.complexity
  private final def foreachUntil(f: A => Boolean): Unit = {
    var c: Chain[A] = this
    val rights = new collection.mutable.ArrayBuffer[Chain[A]]

    while (c ne null) {
      c match {
        case Singleton(a) =>
          val b = f(a)
          if (b) return ();
          c =
            if (rights.isEmpty) Empty
            else rights.reduceLeft((x, y) => Append(y, x))
          rights.clear()
        case Append(l, r) => c = l; rights += r
        case Wrap(seq) =>
          val iterator = seq.iterator
          while (iterator.hasNext) {
            val b = f(iterator.next)
            if (b) return ()
          }
          c =
            if (rights.isEmpty) Empty
            else rights.reduceLeft((x, y) => Append(y, x))
          rights.clear()
        case Empty =>
          if (rights.isEmpty) {
            c = null
          } else {
            c = rights.last
            rights.trimEnd(1)
          }
      }
    }
  }
  // scalastyle:on null return cyclomatic.complexity


  final def iterator: Iterator[A] = this match {
    case Wrap(seq) => seq.iterator
    case _ => new ChainIterator[A](this)
  }

  final def reverseIterator: Iterator[A] = this match {
    case Wrap(seq) => seq.reverseIterator
    case _ => new ChainReverseIterator[A](this)
  }

  /** Returns the number of elements in this structure */
  final def length: Long = {
    val iter = iterator
    var i: Long = 0L
    while(iter.hasNext) { i += 1; iter.next; }
    i
  }

  /** Alias for length */
  final def size: Long = length


  /** Converts to a list. */
  final def toList: List[A] =
    iterator.toList

  /** Converts to a vector. */
  final def toVector: Vector[A] =
    iterator.toVector

  def show[AA >: A](implicit AA: Show[AA]): String = {
    val builder = new StringBuilder("Chain(")
    var first = true

    foreach { a =>
      if (first) { builder ++= AA.show(a); first = false }
      else builder ++= ", " + AA.show(a)
      ()
    }
    builder += ')'
    builder.result
  }

  def ===[AA >: A](y: Chain[AA])(implicit A: Eq[AA]): Boolean =
    (this eq y) || Eq[List[AA]].eqv(toList, y.toList)

  override def toString: String = show(Show.show[A](_.toString))
}

object Chain extends ChainInstances {

  private val sentinel: Function1[Any, Any] = new scala.runtime.AbstractFunction1[Any, Any]{ def apply(a: Any) = this }

  private[data] final case object Empty extends Chain[Nothing] {
    def isEmpty: Boolean = true
  }
  private[data] final case class Singleton[A](a: A) extends Chain[A] {
    def isEmpty: Boolean = false
  }
  private[data] final case class Append[A](left: Chain[A], right: Chain[A])
    extends Chain[A] {
    def isEmpty: Boolean =
      false // b/c `append` constructor doesn't allow either branch to be empty
  }
  private[data] final case class Wrap[A](seq: Seq[A]) extends Chain[A] {
    override def isEmpty: Boolean =
      false // b/c `fromSeq` constructor doesn't allow either branch to be empty
  }

  /** Empty Chain. */
  val nil: Chain[Nothing] = Empty

  def empty[A]: Chain[A] = nil

  /** Creates a Chain of 1 element. */
  def one[A](a: A): Chain[A] = Singleton(a)

  /** Appends two Chains. */
  def append[A](c: Chain[A], c2: Chain[A]): Chain[A] =
    if (c.isEmpty) c2
    else if (c2.isEmpty) c
    else Append(c, c2)

  /** Creates a Chain from the specified sequence. */
  def fromSeq[A](s: Seq[A]): Chain[A] =
    if (s.isEmpty) nil
    else if (s.lengthCompare(1) == 0) one(s.head)
    else Wrap(s)

  /** Creates a Chain from the specified elements. */
  def apply[A](as: A*): Chain[A] =
    as match {
      case w: collection.mutable.WrappedArray[A] =>
        if (w.isEmpty) nil
        else if (w.size == 1) one(w.head)
        else {
          val arr: Array[A] = w.array
          var c: Chain[A] = one(arr.last)
          var idx = arr.size - 2
          while (idx >= 0) {
            c = Append(one(arr(idx)), c)
            idx -= 1
          }
          c
        }
      case _ => fromSeq(as)
    }

  // scalastyle:off null
  class ChainIterator[A](self: Chain[A]) extends Iterator[A] {
    var c: Chain[A] = if (self.isEmpty) null else self
    val rights = new collection.mutable.ArrayBuffer[Chain[A]]
    var currentIterator: Iterator[A] = null

    override def hasNext: Boolean = (c ne null) || ((currentIterator ne null) && currentIterator.hasNext)

    override def next(): A = {
      @tailrec def go: A =
        if ((currentIterator ne null) && currentIterator.hasNext)
          currentIterator.next()
        else {
          currentIterator = null

          c match {
            case Singleton(a) =>
              c =
                if (rights.isEmpty) null
                else rights.reduceLeft((x, y) => Append(y, x))
              rights.clear()
              a
            case Append(l, r) =>
              c = l
              rights += r
              go
            case Wrap(seq) =>
              c =
                if (rights.isEmpty) null
                else rights.reduceLeft((x, y) => Append(y, x))
              rights.clear()
              currentIterator = seq.iterator
              currentIterator.next
            case Empty =>
              go // This shouldn't happen
          }
        }

      go
    }
  }
  // scalastyle:on null


  // scalastyle:off null
  class ChainReverseIterator[A](self: Chain[A]) extends Iterator[A] {
    var c: Chain[A] = if (self.isEmpty) null else self
    val lefts = new collection.mutable.ArrayBuffer[Chain[A]]
    var currentIterator: Iterator[A] = null

    override def hasNext: Boolean = (c ne null) || ((currentIterator ne null) && currentIterator.hasNext)

    override def next(): A = {
      @tailrec def go: A =
        if ((currentIterator ne null) && currentIterator.hasNext)
          currentIterator.next()
        else {
          currentIterator = null

          c match {
            case Singleton(a) =>
              c =
                if (lefts.isEmpty) null
                else lefts.reduceLeft((x, y) => Append(x, y))
              lefts.clear()
              a
            case Append(l, r) =>
              c = r
              lefts += l
              go
            case Wrap(seq) =>
              c =
                if (lefts.isEmpty) null
                else lefts.reduceLeft((x, y) => Append(x, y))
              lefts.clear()
              currentIterator = seq.reverseIterator
              currentIterator.next
            case Empty =>
              go // This shouldn't happen
          }
        }

      go
    }
  }
  // scalastyle:on null
}

private[data] sealed abstract class ChainInstances {
  implicit def catsDataMonoidForChain[A]: Monoid[Chain[A]] = new Monoid[Chain[A]] {
    def empty: Chain[A] = Chain.nil
    def combine(c: Chain[A], c2: Chain[A]): Chain[A] = Chain.append(c, c2)
  }

  implicit val catsDataInstancesForChain: Traverse[Chain] with Alternative[Chain] with Monad[Chain] =
    new Traverse[Chain] with Alternative[Chain] with Monad[Chain] {
      def foldLeft[A, B](fa: Chain[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)
      def foldRight[A, B](fa: Chain[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Eval.defer(fa.foldRight(lb) { (a, lb) =>
          Eval.defer(f(a, lb))
        })

      override def map[A, B](fa: Chain[A])(f: A => B): Chain[B] = fa.map(f)
      override def toList[A](fa: Chain[A]): List[A] = fa.toList
      override def isEmpty[A](fa: Chain[A]): Boolean = fa.isEmpty
      override def exists[A](fa: Chain[A])(p: A => Boolean): Boolean = fa.exists(p)
      override def forall[A](fa: Chain[A])(p: A => Boolean): Boolean = fa.forall(p)
      override def find[A](fa: Chain[A])(f: A => Boolean): Option[A] = fa.find(f)
      override def size[A](fa: Chain[A]): Long = fa.size

      def traverse[G[_], A, B](fa: Chain[A])(f: A => G[B])(implicit G: Applicative[G]): G[Chain[B]] =
        fa.foldRight[G[Chain[B]]](G.pure(nil)) { (a, gcatb) =>
          G.map2(f(a), gcatb)(_ +: _)
        }
      def empty[A]: Chain[A] = Chain.nil
      def combineK[A](c: Chain[A], c2: Chain[A]): Chain[A] = Chain.append(c, c2)
      def pure[A](a: A): Chain[A] = Chain.one(a)
      def flatMap[A, B](fa: Chain[A])(f: A => Chain[B]): Chain[B] =
        fa.flatMap(f)
      def tailRecM[A, B](a: A)(f: A => Chain[Either[A, B]]): Chain[B] = {
        var acc: Chain[B] = Chain.nil
        @tailrec def go(rest: List[Chain[Either[A, B]]]): Unit =
          rest match {
            case hd :: tl =>
              hd.uncons match {
                case Some((hdh, hdt)) =>
                  hdh match {
                    case Right(b) =>
                      acc = acc :+ b
                      go(hdt :: tl)
                    case Left(a) =>
                      go(f(a) :: hdt :: tl)
                  }
                case None =>
                  go(tl)
              }
            case _ => ()
          }
        go(f(a) :: Nil)
        acc
      }
    }

  implicit def catsDataShowForChain[A](implicit A: Show[A]): Show[Chain[A]] =
    Show.show[Chain[A]](_.show)

  implicit def catsDataEqForChain[A](implicit A: Eq[A]): Eq[Chain[A]] = new Eq[Chain[A]] {
    def eqv(x: Chain[A], y: Chain[A]): Boolean =
      (x eq y) || x.toList === y.toList
  }

}
