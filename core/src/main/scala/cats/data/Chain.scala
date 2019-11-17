package cats
package data

import Chain._
import cats.kernel.instances.StaticMethods

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.collection.immutable.TreeSet
import scala.collection.mutable.ListBuffer

/**
 * Trivial catenable sequence. Supports O(1) append, and (amortized)
 * O(1) `uncons`, such that walking the sequence via N successive `uncons`
 * steps takes O(N).
 */
sealed abstract class Chain[+A] {

  /**
   * Returns the head and tail of this Chain if non empty, none otherwise. Amortized O(1).
   */
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
          val tail = fromSeq(seq.tail)
          val next =
            if (rights.isEmpty) tail
            else tail ++ rights.reduceLeft((x, y) => Append(y, x))
          result = Some((seq.head, next))
        case Empty =>
          // Empty is only top level, it is never internal to an Append
          result = None
      }
    }
    // scalastyle:on null
    result
  }

  /**
   * Returns the init and last of this Chain if non empty, none otherwise. Amortized O(1).
   */
  final def initLast: Option[(Chain[A], A)] = {
    var c: Chain[A] = this
    val lefts = new collection.mutable.ArrayBuffer[Chain[A]]
    // scalastyle:off null
    var result: Option[(Chain[A], A)] = null
    while (result eq null) {
      c match {
        case Singleton(a) =>
          val pre =
            if (lefts.isEmpty) nil
            else lefts.reduceLeft((x, y) => Append(x, y))
          result = Some(pre -> a)
        case Append(l, r) => c = r; lefts += l
        case Wrap(seq) =>
          val init = fromSeq(seq.init)
          val pre =
            if (lefts.isEmpty) init
            else lefts.reduceLeft((x, y) => Append(x, y)) ++ init
          result = Some((pre, seq.last))
        case Empty =>
          // Empty is only top level, it is never internal to an Append
          result = None
      }
    }
    // scalastyle:on null
    result
  }

  /**
   * Returns the head of this Chain if non empty, none otherwise. Amortized O(1).
   */
  def headOption: Option[A] = uncons.map(_._1)

  /**
   * Returns the last of this Chain if non empty, none otherwise. Amortized O(1).
   */
  final def lastOption: Option[A] = initLast.map(_._2)

  /**
   * Returns true if there are no elements in this collection.
   */
  def isEmpty: Boolean

  /**
   * Returns false if there are no elements in this collection.
   */
  final def nonEmpty: Boolean = !isEmpty

  /**
   * Concatenates this with `c` in O(1) runtime.
   */
  final def concat[A2 >: A](c: Chain[A2]): Chain[A2] =
    Chain.concat(this, c)

  /**
   * Alias for concat
   */
  final def ++[A2 >: A](c: Chain[A2]): Chain[A2] =
    concat(c)

  /**
   * Returns a new Chain consisting of `a` followed by this. O(1) runtime.
   */
  final def prepend[A2 >: A](a: A2): Chain[A2] =
    Chain.concat(one(a), this)

  /**
   * Alias for [[prepend]].
   */
  final def +:[A2 >: A](a: A2): Chain[A2] =
    prepend(a)

  /**
   * Returns a new Chain consisting of this followed by `a`. O(1) runtime.
   */
  final def append[A2 >: A](a: A2): Chain[A2] =
    Chain.concat(this, one(a))

  /**
   * Alias for [[append]].
   */
  final def :+[A2 >: A](a: A2): Chain[A2] =
    append(a)

  /**
   * Applies the supplied function to each element and returns a new Chain.
   */
  final def map[B](f: A => B): Chain[B] = this match {
    case Wrap(seq) => Wrap(seq.map(f))
    case _         => fromSeq(iterator.map(f).toVector)
  }

  /**
   * Applies the supplied function to each element and returns a new Chain from the concatenated results
   */
  final def flatMap[B](f: A => Chain[B]): Chain[B] = {
    var result = empty[B]
    val iter = iterator
    while (iter.hasNext) { result = result ++ f(iter.next) }
    result
  }

  /**
   * Folds over the elements from left to right using the supplied initial value and function.
   */
  final def foldLeft[B](z: B)(f: (B, A) => B): B = {
    var result = z
    val iter = iterator
    while (iter.hasNext) { result = f(result, iter.next) }
    result
  }

  /**
   * Takes longest prefix of elements that satisfy a predicate.
   * @param p The predicate used to test elements.
   * @return the longest prefix of this chain whose elements all satisfy the predicate p.
   */
  final def takeWhile(p: A => Boolean): Chain[A] = {
    var result = Chain.empty[A]
    foreachUntil { a =>
      val pr = p(a)
      if (pr) result = result :+ a
      !pr
    }
    result
  }

  /**
   * Drops longest prefix of elements that satisfy a predicate.
   *
   * @param p The predicate used to test elements.
   * @return the longest suffix of this sequence whose first element does not satisfy the predicate p.
   */
  final def dropWhile(p: A => Boolean): Chain[A] = {
    @tailrec
    def go(rem: Chain[A]): Chain[A] =
      rem.uncons match {
        case Some((a, tail)) =>
          if (p(a)) go(tail)
          else rem

        case None => nil
      }
    go(this)
  }

  /**
   * Folds over the elements from right to left using the supplied initial value and function.
   */
  final def foldRight[B](z: B)(f: (A, B) => B): B = {
    var result = z
    val iter = reverseIterator
    while (iter.hasNext) { result = f(iter.next, result) }
    result
  }

  /**
   * Collect `B` from this for which `f` is defined
   */
  final def collect[B](pf: PartialFunction[A, B]): Chain[B] =
    foldLeft(Chain.nil: Chain[B]) { (acc, a) =>
      // trick from TraversableOnce, used to avoid calling both isDefined and apply (or calling lift)
      val x = pf.applyOrElse(a, sentinel)
      if (x.asInstanceOf[AnyRef] ne sentinel) acc :+ x.asInstanceOf[B]
      else acc
    }

  /**
   * Finds the first element of this `Chain` for which the given partial
   * function is defined, and applies the partial function to it.
   */
  final def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = {
    var result: Option[B] = None
    foreachUntil { a =>
      // trick from TraversableOnce, used to avoid calling both isDefined and apply (or calling lift)
      val x = pf.applyOrElse(a, sentinel)
      if (x.asInstanceOf[AnyRef] ne sentinel) {
        result = Some(x.asInstanceOf[B])
        true
      } else false
    }
    result
  }

  /**
   * Like `collectFirst` from `scala.collection.Traversable` but takes `A => Option[B]`
   * instead of `PartialFunction`s.
   */
  final def collectFirstSome[B](f: A => Option[B]): Option[B] = {
    var result: Option[B] = None
    foreachUntil { a =>
      val x = f(a)
      if (x.isDefined) {
        result = x
        true
      } else false
    }
    result
  }

  /**
   * Remove elements not matching the predicate
   */
  final def filter(f: A => Boolean): Chain[A] =
    collect { case a if f(a) => a }

  /**
   * Remove elements matching the predicate
   */
  final def filterNot(f: A => Boolean): Chain[A] =
    filter(a => !f(a))

  /**
   * Find the first element matching the predicate, if one exists
   */
  final def find(f: A => Boolean): Option[A] = {
    var result: Option[A] = Option.empty[A]
    foreachUntil { a =>
      val b = f(a)
      if (b) result = Option(a)
      b
    }
    result
  }

  /**
   * Check whether at least one element satisfies the predicate
   */
  final def exists(f: A => Boolean): Boolean = {
    var result: Boolean = false
    foreachUntil { a =>
      val b = f(a)
      if (b) result = true
      b
    }
    result
  }

  /**
   * Check whether all elements satisfy the predicate
   */
  final def forall(f: A => Boolean): Boolean = {
    var result: Boolean = true
    foreachUntil { a =>
      val b = f(a)
      if (!b) result = false
      !b
    }
    result
  }

  /**
   * Check whether an element is in this structure
   */
  final def contains[AA >: A](a: AA)(implicit A: Eq[AA]): Boolean =
    exists(A.eqv(a, _))

  /**
   * Zips this `Chain` with another `Chain` and applies a function for each pair of elements.
   */
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
        case None      => m += ((k, NonEmptyChain.one(elem))); ()
        case Some(cat) => m = m.updated(k, cat :+ elem)
      }
    }
    m
  }

  /**
   * Reverses this `Chain`
   */
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

  /**
   * Applies the supplied function to each element, left to right.
   */
  final private def foreach(f: A => Unit): Unit = foreachUntil { a =>
    f(a); false
  }

  /**
   * Applies the supplied function to each element, left to right, but stops when true is returned
   */
  // scalastyle:off null return cyclomatic.complexity
  final private def foreachUntil(f: A => Boolean): Unit = {
    var c: Chain[A] = this
    val rights = new collection.mutable.ArrayBuffer[Chain[A]]

    while (c ne null) {
      c match {
        case Singleton(a) =>
          val b = f(a)
          if (b) return ()
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
          // Empty is only top level, it is never internal to an Append
          c = null
      }
    }
  }
  // scalastyle:on null return cyclomatic.complexity

  final def iterator: Iterator[A] = this match {
    case Wrap(seq) => seq.iterator
    case _         => new ChainIterator[A](this)
  }

  final def reverseIterator: Iterator[A] = this match {
    case Wrap(seq) => seq.reverseIterator
    case _         => new ChainReverseIterator[A](this)
  }

  /**
   * Returns the number of elements in this structure
   */
  final def length: Long = {
    val iter = iterator
    var i: Long = 0
    while (iter.hasNext) { i += 1; iter.next; }
    i
  }

  /**
   * Alias for length
   */
  final def size: Long = length

  /**
   * Converts to a list.
   */
  final def toList: List[A] =
    iterator.toList

  /**
   * Converts to a vector.
   */
  final def toVector: Vector[A] =
    iterator.toVector

  /**
   * Typesafe equality operator.
   *
   * This method is similar to == except that it only allows two
   * Chain[A] values to be compared to each other, and uses
   * equality provided by Eq[_] instances, rather than using the
   * universal equality provided by .equals.
   */
  def ===[AA >: A](that: Chain[AA])(implicit A: Eq[AA]): Boolean =
    (this eq that) || {
      val iterX = iterator
      val iterY = that.iterator
      while (iterX.hasNext && iterY.hasNext) {
        // scalastyle:off return
        if (!A.eqv(iterX.next, iterY.next)) return false
        // scalastyle:on return
      }

      iterX.hasNext == iterY.hasNext
    }

  /**
   * Remove duplicates. Duplicates are checked using `Order[_]` instance.
   */
  def distinct[AA >: A](implicit O: Order[AA]): Chain[AA] = {
    implicit val ord = O.toOrdering

    var alreadyIn = TreeSet.empty[AA]

    foldLeft(Chain.empty[AA]) { (elementsSoFar, b) =>
      if (alreadyIn.contains(b)) {
        elementsSoFar
      } else {
        alreadyIn += b
        elementsSoFar :+ b
      }
    }
  }

  def show[AA >: A](implicit AA: Show[AA]): String = {
    val builder = new StringBuilder("Chain(")
    var first = true

    foreach { a =>
      if (first) {
        builder ++= AA.show(a); first = false
      } else builder ++= ", " + AA.show(a)
      ()
    }
    builder += ')'
    builder.result
  }

  def hash[AA >: A](implicit hashA: Hash[AA]): Int = StaticMethods.orderedHash((this: Chain[AA]).iterator)

  override def toString: String = show(Show.show[A](_.toString))

  override def equals(o: Any): Boolean =
    if (o.isInstanceOf[Chain[_]])
      (this: Chain[Any]).===(o.asInstanceOf[Chain[Any]])(Eq.fromUniversalEquals[Any])
    else false

  override def hashCode: Int = hash(Hash.fromUniversalHashCode[A])

  final def get(idx: Long): Option[A] =
    if (idx < 0) None
    else {
      var result: Option[A] = None
      var i = 0L
      foreachUntil { a =>
        if (idx == i) {
          result = Some(a)
          true
        } else {
          i += 1
          false
        }
      }
      result
    }
}

object Chain extends ChainInstances {

  private val sentinel: Function1[Any, Any] = new scala.runtime.AbstractFunction1[Any, Any] { def apply(a: Any) = this }

  final private[data] case object Empty extends Chain[Nothing] {
    def isEmpty: Boolean = true
  }
  final private[data] case class Singleton[A](a: A) extends Chain[A] {
    def isEmpty: Boolean = false
  }
  final private[data] case class Append[A](left: Chain[A], right: Chain[A]) extends Chain[A] {
    def isEmpty: Boolean =
      false // b/c `concat` constructor doesn't allow either branch to be empty
  }
  final private[data] case class Wrap[A](seq: Seq[A]) extends Chain[A] {
    override def isEmpty: Boolean =
      false // b/c `fromSeq` constructor doesn't allow either branch to be empty
  }

  def unapplySeq[A](chain: Chain[A]): Option[Seq[A]] =
    Some(chain.toList)

  object ==: {
    def unapply[T](c: Chain[T]): Option[(T, Chain[T])] =
      c.uncons
  }

  object :== {
    def unapply[T](c: Chain[T]): Option[(Chain[T], T)] =
      c.initLast
  }

  /** Empty Chain. */
  val nil: Chain[Nothing] = Empty

  def empty[A]: Chain[A] = nil

  /** Creates a Chain of 1 element. */
  def one[A](a: A): Chain[A] = Singleton(a)

  /** Concatenates two Chains. */
  def concat[A](c: Chain[A], c2: Chain[A]): Chain[A] =
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
    fromSeq(as)

  // scalastyle:off null
  private class ChainIterator[A](self: Chain[A]) extends Iterator[A] {
    private[this] var c: Chain[A] = if (self.isEmpty) null else self
    private[this] val rights = new collection.mutable.ArrayBuffer[Chain[A]]
    private[this] var currentIterator: Iterator[A] = null

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
            case null | Empty =>
              throw new java.util.NoSuchElementException("next called on empty iterator")
          }
        }

      go
    }
  }
  // scalastyle:on null

  // scalastyle:off null
  private class ChainReverseIterator[A](self: Chain[A]) extends Iterator[A] {
    private[this] var c: Chain[A] = if (self.isEmpty) null else self
    private[this] val lefts = new collection.mutable.ArrayBuffer[Chain[A]]
    private[this] var currentIterator: Iterator[A] = null

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
            case null | Empty =>
              throw new java.util.NoSuchElementException("next called on empty iterator")
          }
        }

      go
    }
  }
  // scalastyle:on null
}

sealed abstract private[data] class ChainInstances extends ChainInstances1 {
  implicit def catsDataMonoidForChain[A]: Monoid[Chain[A]] = new Monoid[Chain[A]] {
    def empty: Chain[A] = Chain.nil
    def combine(c: Chain[A], c2: Chain[A]): Chain[A] = Chain.concat(c, c2)
  }

  implicit val catsDataInstancesForChain
    : Traverse[Chain] with Alternative[Chain] with Monad[Chain] with CoflatMap[Chain] with Align[Chain] =
    new Traverse[Chain] with Alternative[Chain] with Monad[Chain] with CoflatMap[Chain] with Align[Chain] {
      def foldLeft[A, B](fa: Chain[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)
      def foldRight[A, B](fa: Chain[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        def loop(as: Chain[A]): Eval[B] =
          as match {
            case Chain.nil => lb
            case h ==: t   => f(h, Eval.defer(loop(t)))
          }
        Eval.defer(loop(fa))
      }

      override def map[A, B](fa: Chain[A])(f: A => B): Chain[B] = fa.map(f)
      override def toList[A](fa: Chain[A]): List[A] = fa.toList
      override def isEmpty[A](fa: Chain[A]): Boolean = fa.isEmpty
      override def exists[A](fa: Chain[A])(p: A => Boolean): Boolean = fa.exists(p)
      override def forall[A](fa: Chain[A])(p: A => Boolean): Boolean = fa.forall(p)
      override def find[A](fa: Chain[A])(f: A => Boolean): Option[A] = fa.find(f)
      override def size[A](fa: Chain[A]): Long = fa.length
      override def collectFirst[A, B](fa: Chain[A])(pf: PartialFunction[A, B]): Option[B] = fa.collectFirst(pf)
      override def collectFirstSome[A, B](fa: Chain[A])(f: A => Option[B]): Option[B] = fa.collectFirstSome(f)

      def coflatMap[A, B](fa: Chain[A])(f: Chain[A] => B): Chain[B] = {
        @tailrec def go(as: Chain[A], res: ListBuffer[B]): Chain[B] =
          as.uncons match {
            case Some((_, t)) => go(t, res += f(as))
            case None         => Chain.fromSeq(res.result())
          }

        go(fa, ListBuffer.empty)
      }

      def traverse[G[_], A, B](fa: Chain[A])(f: A => G[B])(implicit G: Applicative[G]): G[Chain[B]] =
        foldRight[A, G[Chain[B]]](fa, Always(G.pure(nil))) { (a, lglb) =>
          G.map2Eval(f(a), lglb)(_ +: _)
        }.value
      def empty[A]: Chain[A] = Chain.nil
      def combineK[A](c: Chain[A], c2: Chain[A]): Chain[A] = Chain.concat(c, c2)
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

      override def get[A](fa: Chain[A])(idx: Long): Option[A] = fa.get(idx)

      def functor: Functor[Chain] = this

      def align[A, B](fa: Chain[A], fb: Chain[B]): Chain[Ior[A, B]] =
        alignWith(fa, fb)(identity)

      override def alignWith[A, B, C](fa: Chain[A], fb: Chain[B])(f: Ior[A, B] => C): Chain[C] = {
        val iterA = fa.iterator
        val iterB = fb.iterator

        var result: Chain[C] = Chain.empty

        while (iterA.hasNext || iterB.hasNext) {
          val ior =
            if (iterA.hasNext && iterB.hasNext) Ior.both(iterA.next(), iterB.next())
            else if (iterA.hasNext) Ior.left(iterA.next())
            else Ior.right(iterB.next())
          result = result :+ f(ior)
        }
        result
      }
    }

  implicit def catsDataShowForChain[A](implicit A: Show[A]): Show[Chain[A]] =
    Show.show[Chain[A]](_.show)

  implicit def catsDataOrderForChain[A](implicit A0: Order[A]): Order[Chain[A]] =
    new Order[Chain[A]] with ChainPartialOrder[A] {
      implicit def A: PartialOrder[A] = A0
      def compare(x: Chain[A], y: Chain[A]): Int =
        if (x eq y) 0
        else {
          val iterX = x.iterator
          val iterY = y.iterator
          while (iterX.hasNext && iterY.hasNext) {
            val n = A0.compare(iterX.next, iterY.next)
            // scalastyle:off return
            if (n != 0) return n
            // scalastyle:on return
          }

          if (iterX.hasNext) 1
          else if (iterY.hasNext) -1
          else 0
        }
    }

  implicit val catsDataTraverseFilterForChain: TraverseFilter[Chain] = new TraverseFilter[Chain] {
    def traverse: Traverse[Chain] = Chain.catsDataInstancesForChain

    override def filter[A](fa: Chain[A])(f: A => Boolean): Chain[A] = fa.filter(f)

    override def filterNot[A](fa: Chain[A])(f: A => Boolean): Chain[A] = fa.filterNot(f)

    override def collect[A, B](fa: Chain[A])(f: PartialFunction[A, B]): Chain[B] = fa.collect(f)

    override def mapFilter[A, B](fa: Chain[A])(f: A => Option[B]): Chain[B] = fa.collect(Function.unlift(f))

    override def flattenOption[A](fa: Chain[Option[A]]): Chain[A] = fa.collect { case Some(a) => a }

    def traverseFilter[G[_], A, B](fa: Chain[A])(f: A => G[Option[B]])(implicit G: Applicative[G]): G[Chain[B]] =
      traverse
        .foldRight(fa, Eval.now(G.pure(Chain.empty[B])))(
          (x, xse) => G.map2Eval(f(x), xse)((i, o) => i.fold(o)(_ +: o))
        )
        .value

    override def filterA[G[_], A](fa: Chain[A])(f: A => G[Boolean])(implicit G: Applicative[G]): G[Chain[A]] =
      traverse
        .foldRight(fa, Eval.now(G.pure(Chain.empty[A])))(
          (x, xse) => G.map2Eval(f(x), xse)((b, chain) => if (b) x +: chain else chain)
        )
        .value

  }

}

sealed abstract private[data] class ChainInstances1 extends ChainInstances2 {
  implicit def catsDataPartialOrderForChain[A](implicit A0: PartialOrder[A]): PartialOrder[Chain[A]] =
    new ChainPartialOrder[A] { implicit def A: PartialOrder[A] = A0 }
}

sealed abstract private[data] class ChainInstances2 extends ChainInstances3 {
  implicit def catsDataHashForChain[A](implicit A: Hash[A]): Hash[Chain[A]] = new Hash[Chain[A]] {
    def eqv(x: Chain[A], y: Chain[A]): Boolean = x === y

    def hash(fa: Chain[A]): Int = fa.hash
  }
}

sealed abstract private[data] class ChainInstances3 {
  implicit def catsDataEqForChain[A](implicit A: Eq[A]): Eq[Chain[A]] = new Eq[Chain[A]] {
    def eqv(x: Chain[A], y: Chain[A]): Boolean = x === y
  }
}

private[data] trait ChainPartialOrder[A] extends PartialOrder[Chain[A]] {
  implicit def A: PartialOrder[A]

  override def partialCompare(x: Chain[A], y: Chain[A]): Double =
    if (x eq y) 0.0
    else {
      val iterX = x.iterator
      val iterY = y.iterator
      while (iterX.hasNext && iterY.hasNext) {
        val n = A.partialCompare(iterX.next, iterY.next)
        // scalastyle:off return
        if (n != 0.0) return n
        // scalastyle:on return
      }

      if (iterX.hasNext) 1.0
      else if (iterY.hasNext) -1.0
      else 0.0
    }

  override def eqv(x: Chain[A], y: Chain[A]): Boolean = x === y
}
