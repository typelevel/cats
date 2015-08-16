package cats
package data

import cats.syntax.order._
import scala.reflect.ClassTag

import scala.annotation.tailrec
import scala.collection.mutable

sealed abstract class Stream[A] { lhs =>

  import Stream.{Empty, Next, This}

  /**
   * The stream's catamorphism.
   *
   * This method allows the stream to be transformed to an abtirary
   * by handling two cases:
   *
   *  1. empty stream: return b
   *  2. non-empty stream: apply the function to the head and tail
   *
   * This method can be more convenient than pattern-matching, since
   * it includes support for handling deferred streams (i.e. Next(_)),
   * these nodes will be evaluated until an empty or non-empty stream
   * is found (i.e. until Empty() or This() is found).
   */
  def fold[B](b: => B, f: (A, Eval[Stream[A]]) => B): B = {
    @tailrec def unroll(s: Stream[A]): B =
      s match {
        case Empty() => b
        case Next(lt) => unroll(lt.value)
        case This(a, lt) => f(a, lt)
      }
    unroll(this)
  }

  /**
   * A variant of fold, used for constructing streams.
   *
   * The only difference is that foldStream will preserve deferred
   * streams. This makes it more appropriate to use in situations
   * where the stream's laziness must be preserved.
   */
  def foldStream[B](bs: => Stream[B], f: (A, Eval[Stream[A]]) => Stream[B]): Stream[B] =
    this match {
      case Empty() => bs
      case Next(lt) => Next(lt.map(_.foldStream(bs, f)))
      case This(a, lt) => f(a, lt)
    }

  /**
   * Deconstruct a stream into a head and tail (if available).
   *
   * This method will evaluate the stream until it finds a head and
   * tail, or until the stream is exhausted.
   */
  def uncons: Option[(A, Eval[Stream[A]])] = {
    @tailrec def unroll(s: Stream[A]): Option[(A, Eval[Stream[A]])] =
      s match {
        case Empty() => None
        case Next(lt) => unroll(lt.value)
        case This(a, lt) => Some((a, lt))
      }
    unroll(this)
  }

  /**
   * Lazily transform the stream given a function `f`.
   */
  def map[B](f: A => B): Stream[B] =
    this match {
      case Empty() => Empty()
      case Next(lt) => Next(lt.map(_.map(f)))
      case This(a, lt) => This(f(a), lt.map(_.map(f)))
    }

  /**
   * Lazily transform the stream given a function `f`.
   */
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    this match {
      case Empty() => Empty()
      case Next(lt) => Next(lt.map(_.flatMap(f)))
      case This(a, lt) => f(a) concat lt.map(_.flatMap(f))
    }

  /**
   * Lazily filter the stream given the predicate `f`.
   */
  def filter(f: A => Boolean): Stream[A] =
    this match {
      case Empty() => this
      case Next(lt) => Next(lt.map(_.filter(f)))
      case This(a, lt) => if (f(a)) this else Next(lt.map(_.filter(f)))
    }

  /**
   * Eagerly fold the stream to a single value from the left.
   */
  def foldLeft[B](b: B)(f: (B, A) => B): B = {
    @tailrec def unroll(s: Stream[A], b: B): B =
      s match {
        case Empty() => b
        case Next(lt) => unroll(lt.value, b)
        case This(a, lt) => unroll(lt.value, f(b, a))
      }
    unroll(this, b)
  }

  /**
   * Lazily fold the stream to a single value from the right.
   */
  def foldRight[B](b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    this match {
      case Empty() => b
      case Next(lt) => lt.flatMap(_.foldRight(b)(f))
      case This(a, lt) => f(a, lt.flatMap(_.foldRight(b)(f)))
    }

  /**
   * Return true if the stream is empty, false otherwise.
   *
   * In this case of deferred streams this will force the first
   * element to be calculated.
   */
  def isEmpty: Boolean = {
    @tailrec def unroll(s: Stream[A]): Boolean =
      s match {
        case This(_, _) => false
        case Empty() => true
        case Next(lt) => unroll(lt.value)
      }
    unroll(this)
  }

  /**
   * Return true if the stream is non-empty, false otherwise.
   *
   * In this case of deferred streams this will force the first
   * element to be calculated.
   */
  def nonEmpty: Boolean =
    !isEmpty

  /**
   * Peek at the start of the stream to see whether we know if it is
   * empty.
   *
   * Unlike .isEmpty/.nonEmpty, this method will not force the
   * calculationg of a deferred stream. Instead, None will be
   * returned.
   */
  def peekEmpty: Option[Boolean] =
    this match {
      case Empty() => Some(true)
      case Next(_) => None
      case This(a, lt) => Some(false)
    }

  /**
   * Lazily concatenate two streams.
   */
  def concat(rhs: Stream[A]): Stream[A] =
    this match {
      case Empty() => rhs
      case Next(lt) => Next(lt.map(_ concat rhs))
      case This(a, lt) => This(a, lt.map(_ concat rhs))
    }

  /**
   * Lazily concatenate two streams.
   *
   * In this case the evaluation of the second stream may be deferred.
   */
  def concat(rhs: Eval[Stream[A]]): Stream[A] =
    this match {
      case Empty() => Next(rhs)
      case Next(lt) => Next(lt.map(_ concat rhs))
      case This(a, lt) => This(a, lt.map(_ concat rhs))
    }

  /**
   * Lazily zip two streams together.
   *
   * The lenght of the result will be the shorter of the two
   * arguments.
   */
  def zip[B](rhs: Stream[B]): Stream[(A, B)] =
    (lhs.uncons, rhs.uncons) match {
      case (Some((a, lta)), Some((b, ltb))) =>
        This((a, b), Always(lta.value zip ltb.value))
      case _ =>
        Empty()
    }

  def zipWithIndex: Stream[(A, Int)] = {
    def loop(s: Stream[A], i: Int): Stream[(A, Int)] =
      s match {
        case Empty() => Empty()
        case Next(lt) => Next(lt.map(s => loop(s, i)))
        case This(a, lt) => This((a, i), lt.map(s => loop(s, i + 1)))
      }
    loop(this, 0)
  }

  /**
   * Lazily zip two streams together using Ior.
   *
   * Unlike `zip`, the length of the result will be the longer of the
   * two arguments.
   */
  def izip[B](rhs: Stream[B]): Stream[Ior[A, B]] =
    (lhs.uncons, rhs.uncons) match {
      case (Some((a, lta)), Some((b, ltb))) =>
        This(Ior.both(a, b), Always(lta.value izip ltb.value))
      case (Some(_), None) =>
        lhs.map(a => Ior.left(a))
      case (None, Some(_)) =>
        rhs.map(b => Ior.right(b))
      case _ =>
        Empty()
    }

  /**
   * Unzip this stream of tuples into two distinct streams.
   */
  def unzip[B, C](implicit ev: A =:= (B, C)): (Stream[B], Stream[C]) =
    (this.map(_._1), this.map(_._2))

  /**
   * Merge two sorted streams into a new stream.
   *
   * The streams are assumed to already be sorted. If they are not,
   * the resulting order is not defined.
   */
  def merge(rhs: Stream[A])(implicit ev: Order[A]): Stream[A] =
    (lhs.uncons, rhs.uncons) match {
      case (Some((a0, lt0)), Some((a1, lt1))) =>
        if (a0 < a1) This(a0, Always(lt0.value merge rhs))
        else This(a1, Always(lhs merge lt1.value))
      case (None, None) => Empty()
      case (_, None) => lhs
      case (None, _) => rhs
    }

  /**
   * Interleave the elements of two streams.
   *
   * Given x = [x0, x1, x2, ...] and y = [y0, y1, y2, ...] this method
   * will return the stream [x0, y0, x1, y1, x2, ...]
   *
   * If one stream is longer than the other, the rest of its elements
   * will appear after the other stream is exhausted.
   */
  def interleave(rhs: Stream[A]): Stream[A] =
    lhs.uncons match {
      case None => rhs
      case Some((a, lt)) => This(a, Always(rhs interleave lt.value))
    }

  /**
   * Produce the Cartestian product of two streams.
   *
   * Given x = [x0, x1, x2, ...] and y = [y0, y1, y2, ...] this method
   * will return the stream:
   *
   *   [(x0, y0), (x0, y1), (x1, y0), (x0, y2), (x1, y1), (x2, y0), ...]
   *
   * This is the diagonalized product of both streams. Every possible
   * combination will (eventually) be reached.
   *
   * This is true even for infinite streams, at least in theory --
   * time and space limitations of evaluating an infinite stream may
   * make it impossible to reach very distant elements.
   */
  def product[B](rhs: Stream[B]): Stream[(A, B)] = {
    def loop(i: Int): Stream[(A, B)] = {
      val xs = lhs.take(i + 1).asInstanceOf[Stream[AnyRef]].toArray
      val ys = rhs.take(i + 1).asInstanceOf[Stream[AnyRef]].toArray
      def build(j: Int): Stream[(A, B)] =
        if (j > i) Empty() else {
          val k = i - j
          if (j >= xs.length || k >= ys.length) build(j + 1) else {
            val tpl = (xs(j).asInstanceOf[A], ys(k).asInstanceOf[B])
            This(tpl, Always(build(j + 1)))
          }
        }
      if (i > xs.length + ys.length - 2) Empty() else {
        build(0) concat Always(loop(i + 1))
      }
    }
    loop(0)
  }

  /**
   * Return true if some element of the stream satisfies the
   * predicate, false otherwise.
   */
  def exists(f: A => Boolean): Boolean = {
    @tailrec def unroll(s: Stream[A]): Boolean =
      s match {
        case Empty() => false
        case Next(lt) => unroll(lt.value)
        case This(a, lt) => if (f(a)) true else unroll(lt.value)
      }
    unroll(this)
  }

  /**
   * Return true if every element of the stream satisfies the
   * predicate, false otherwise.
   */
  def forall(f: A => Boolean): Boolean = {
    @tailrec def unroll(s: Stream[A]): Boolean =
      s match {
        case Empty() => true
        case Next(lt) => unroll(lt.value)
        case This(a, lt) => if (f(a)) unroll(lt.value) else false
      }
    unroll(this)
  }

  /**
   * Return a stream consisting only of the first `n` elements of this
   * stream.
   *
   * If the current stream has `n` or fewer elements, the entire
   * stream will be returned.
   */
  def take(n: Int): Stream[A] =
    if (n <= 0) Empty() else this match {
      case Empty() => Empty()
      case Next(lt) => Next(lt.map(_.take(n)))
      case This(a, lt) => This(a, lt.map(_.take(n - 1)))
    }

  /**
   * Return a stream consisting of all but the first `n` elements of
   * this stream.
   *
   * If the current stream has `n` or fewer elements, an empty stream
   * will be returned.
   */
  def drop(n: Int): Stream[A] =
    if (n <= 0) this else this match {
      case Empty() => Empty()
      case Next(lt) => Next(lt.map(_.drop(n)))
      case This(a, lt) => Next(lt.map(_.take(n - 1)))
    }

  /**
   * From the beginning of this stream, create a new stream which
   * takes only those elements that fulfill the predicate `f`. Once an
   * element is found which does not fulfill the predicate, no further
   * elements will be returned.
   *
   * If all elements satisfy `f`, the current stream will be returned.
   * If no elements satisfy `f`, an empty stream will be returned.
   *
   * For example:
   *
   *   Stream(1, 2, 3, 4, 5, 6, 7).takeWhile(n => n != 4)
   *
   * Will result in: Stream(1, 2, 3)
   */
  def takeWhile(f: A => Boolean): Stream[A] =
    this match {
      case Empty() => Empty()
      case Next(lt) => Next(lt.map(_.takeWhile(f)))
      case This(a, lt) => if (f(a)) This(a, lt.map(_.takeWhile(f))) else Empty()
    }

  /**
   * From the beginning of this stream, create a new stream which
   * removes all elements that fulfill the predicate `f`. Once an
   * element is found which does not fulfill the predicate, that
   * element and all subsequent elements will be returned.
   *
   * If all elements satisfy `f`, an empty stream will be returned.
   * If no elements satisfy `f`, the current stream will be returned.
   *
   * For example:
   *
   *   Stream(1, 2, 3, 4, 5, 6, 7).takeWhile(n => n != 4)
   *
   * Will result in: Stream(4, 5, 6, 7)
   */
  def dropWhile(f: A => Boolean): Stream[A] =
    this match {
      case Empty() => Empty()
      case Next(lt) => Next(lt.map(_.dropWhile(f)))
      case This(a, lt) => if (f(a)) Empty() else This(a, lt.map(_.takeWhile(f)))
    }

  /**
   * Provide an iterator over the elements in the stream.
   */
  def iterator: Iterator[A] =
    new Iterator[A] {
      var ls: Eval[Stream[A]] = null
      var s: Stream[A] = lhs
      def hasNext: Boolean =
        { if (s == null) { s = ls.value; ls = null }; s.nonEmpty }
      def next: A = {
        if (s == null) s = ls.value
        s.uncons match {
          case None =>
            throw new NoSuchElementException("next on empty iterator")
          case Some((a, lt)) =>
            { ls = lt; s = null; a }
        }
      }
    }

  /**
   * Provide a list of elements in the stream.
   *
   * This will evaluate the stream immediately, and will hang in the
   * case of infinite streams.
   */
  def toList: List[A] = {
    @tailrec def unroll(buf: mutable.ListBuffer[A], s: Stream[A]): List[A] =
      s match {
        case Empty() => buf.toList
        case Next(lt) => unroll(buf, lt.value)
        case This(a, lt) => unroll(buf += a, lt.value)
      }
    unroll(mutable.ListBuffer.empty[A], this)
  }

  /**
   * Basic string representation of a stream.
   *
   * This method will not force evaluation of any lazy part of a
   * stream. As a result, you will see at most one element (the first
   * one).
   *
   * Use .toString(n) to see the first n elements of the stream.
   */
  override def toString: String =
    this match {
      case This(a, _) => "Stream(" + a + ", ...)"
      case Empty() => "Stream()"
      case Next(_) => "Stream(...)"
    }

  /**
   * String representation of the first n elements of a stream.
   */
  def toString(limit: Int = 10): String = {
    @tailrec def unroll(n: Int, sb: StringBuffer, s: Stream[A]): String =
      if (n <= 0) sb.append(", ...)").toString else s match {
        case Empty() => sb.append(")").toString
        case Next(lt) => unroll(n, sb, lt.value)
        case This(a, lt) => unroll(n - 1, sb.append(", " + a.toString), lt.value)
      }
    uncons match {
      case None =>
        "Stream()"
      case Some((a, lt)) =>
        val sb = new StringBuffer().append("Stream(" + a.toString)
        unroll(limit - 1, sb, lt.value)
    }
  }

  /**
   * Provide an array of elements in the stream.
   *
   * This will evaluate the stream immediately, and will hang in the
   * case of infinite streams.
   */
  def toArray(implicit ct: ClassTag[A]): Array[A] = {
    @tailrec def unroll(buf: mutable.ArrayBuffer[A], s: Stream[A]): Array[A] =
      s match {
        case Empty() => buf.toArray
        case Next(lt) => unroll(buf, lt.value)
        case This(a, lt) => unroll(buf += a, lt.value)
      }
    unroll(mutable.ArrayBuffer.empty[A], this)
  }

  /**
   * Ensure that repeated traversals of the stream will not cause
   * repeated tail computations.
   *
   * By default stream does not memoize to avoid memory leaks when the
   * head of the stream is retained.
   */
  def memoize: Stream[A] =
    this match {
      case Empty() => Empty()
      case Next(lt) => Next(lt.memoize)
      case This(a, lt) => This(a, lt.memoize)
    }

  /**
   * Compact removes "pauses" in the stream (represented as Next(_)
   * nodes).
   *
   * Normally, Next(_) values are used to defer tail computation in
   * cases where it is convenient to return a stream value where
   * neither the head or tail are computed yet.
   *
   * In some cases (particularly if the stream is to be memoized) it
   * may be desirable to ensure that these values are not retained.
   */
  def compact: Stream[A] = {
    @tailrec def unroll(s: Stream[A]): Stream[A] =
      s match {
        case Next(lt) => unroll(lt.value)
        case s => s
      }
    unroll(this)
  }
}

object Stream {

  /**
   * Concrete Stream[A] types:
   *
   *  - Empty(): an empty stream.
   *  - This(a, tail): a non-empty stream containing (at least) `a`.
   *  - Next(tail): a deferred stream.
   *
   * This represents a lazy, possibly infinite stream of values.
   * Eval[_] is used to represent possible laziness (via Now, Later,
   * and Always). The head of `This` is eager -- a lazy head can be
   * represented using `Next(Always(...))` or `Next(Later(...))`.
   */
  case class Empty[A]() extends Stream[A]
  case class Next[A](next: Eval[Stream[A]]) extends Stream[A]
  case class This[A](a: A, tail: Eval[Stream[A]]) extends Stream[A]

  /**
   * Create an empty stream of type A.
   */
  def empty[A]: Stream[A] =
    Empty()

  /**
   * Create a stream consisting of a single value.
   */
  def apply[A](a: A): Stream[A] =
    This(a, Now(Empty()))

  /**
   * Create a stream from two or more values.
   */
  def apply[A](a1: A, a2: A, as: A*): Stream[A] =
    This(a1, Now(This(a2, Now(Stream.fromVector(as.toVector)))))

  /**
   * Defer stream creation.
   *
   * Given an expression which creates a stream, this method defers
   * that creation, allowing the head (if any) to be lazy.
   */
  def defer[A](s: => Stream[A]): Stream[A] =
    Next(Always(s))

  /**
   * Create a stream from a vector.
   *
   * The stream will be eagerly evaluated.
   */
  def fromVector[A](as: Vector[A]): Stream[A] = {
    def loop(s: Stream[A], i: Int): Stream[A] =
      if (i < 0) s else loop(This(as(i), Now(s)), i - 1)
    loop(Empty(), as.length - 1)
  }

  /**
   * Create a stream from a vector.
   *
   * The stream will be eagerly evaluated.
   */
  def fromList[A](as: List[A]): Stream[A] = {
    def loop(s: Stream[A], ras: List[A]): Stream[A] =
      ras match {
        case Nil => s
        case a :: rt => loop(This(a, Now(s)), rt)
      }
    loop(Empty(), as.reverse)
  }

  /**
   * Create a stream from an iterable.
   *
   * The stream will be eagerly evaluated.
   */
  def fromIterable[A](as: Iterable[A]): Stream[A] =
    fromIteratorUnsafe(as.iterator)

  /**
   * Create a stream from an iterator.
   *
   * The stream will be created lazily, to support potentially large
   * (or infinite) iterators. Iterators passed to this method should
   * not be used elsewhere -- doing so will result in problems.
   *
   * The use case for this method is code like .fromIterable, which
   * creates an iterator for the express purpose of calling this
   * method.
   */
  def fromIteratorUnsafe[A](it: Iterator[A]): Stream[A] =
    if (it.hasNext) This(it.next, Later(fromIteratorUnsafe(it))) else Empty()

  /**
   * Create a self-referential stream.
   */
  def knot[A](f: Eval[Stream[A]] => Stream[A], memo: Boolean = false): Stream[A] = {
    lazy val s: Eval[Stream[A]] = if (memo) Later(f(s)) else Always(f(s))
    s.value
  }

  /**
   * Continually return a constant value.
   */
  def continually[A](a: A): Stream[A] =
    knot(s => This(a, s))

  /**
   * Continually return the result of a thunk.
   *
   * This method only differs from `continually` in that the thunk may
   * not be pure. For this reason (and unlike continually), this
   * stream is memoized to ensure that repeated traversals produce the
   * same results.
   */
  def thunk[A](f: () => A): Stream[A] =
    knot(s => This(f(), s), memo = true)

  /**
   * Produce an infinite stream of values given an initial value and a
   * tranformation function.
   */
  def infinite[A](a: A)(f: A => A): Stream[A] =
    This(a, Always(infinite(f(a))(f)))

  /**
   * Stream of integers starting at n.
   */
  def from(n: Int): Stream[Int] =
    infinite(n)(_ + 1)

  /**
   * Provide a stream of integers starting with `start` and ending
   * with `end` (i.e. inclusive).
   */
  def interval(start: Int, end: Int): Stream[Int] =
    if (start > end) Empty() else This(start, Always(interval(start + 1, end)))

  /**
   * Produce a stream given an "unfolding" function.
   *
   * None represents an empty stream. Some(a) reprsents an initial
   * element, and we can compute the tail (if any) via f(a).
   */
  def unfold[A](o: Option[A])(f: A => Option[A]): Stream[A] =
    o match {
      case None => Empty()
      case Some(a) => This(a, Always(unfold(f(a))(f)))
    }

  /**
   * An empty loop, will wait forever if evaluated.
   */
  def godot: Stream[Nothing] =
    knot[Nothing](s => Next[Nothing](s))

  /**
   * Contains various Stream-specific syntax.
   *
   * To eanble this, say:
   *
   *   import cats.data.Stream.syntax._
   *
   * This provides the %:: and %::: operators for constructing Streams
   * lazily, and the %:: extract to use when pattern matching on
   * Streams.
   */
  object syntax {
    object %:: {
      def unapply[A](s: Stream[A]): Option[(A, Eval[Stream[A]])] = s.uncons
    }

    class StreamOps[A](rhs: Eval[Stream[A]]) {
      def %::(lhs: A): Stream[A] = This(lhs, rhs)
      def %:::(lhs: Stream[A]): Stream[A] = lhs concat rhs
    }

    implicit def streamOps[A](as: => Stream[A]): StreamOps[A] =
      new StreamOps(Always(as))
  }
}

trait StreamInstances {
  implicit val streamMonad: MonadCombine[Stream] =
    new MonadCombine[Stream] {
      def pure[A](a: A): Stream[A] =
        Stream(a)
      override def map[A, B](as: Stream[A])(f: A => B): Stream[B] =
        as.map(f)
      def flatMap[A, B](as: Stream[A])(f: A => Stream[B]): Stream[B] =
        as.flatMap(f)
      def empty[A]: Stream[A] =
        Stream.empty
      def combine[A](xs: Stream[A], ys: Stream[A]): Stream[A] =
        xs concat ys
    }
}
