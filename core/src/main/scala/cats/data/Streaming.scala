package cats
package data

import cats.syntax.eq._
import cats.syntax.order._
import scala.reflect.ClassTag

import scala.annotation.tailrec
import scala.collection.mutable

sealed abstract class Streaming[A] { lhs =>

  import Streaming.{Empty, Next, This}

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
  def fold[B](b: => B, f: (A, Eval[Streaming[A]]) => B): B = {
    @tailrec def unroll(s: Streaming[A]): B =
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
  def foldStreaming[B](bs: => Streaming[B], f: (A, Eval[Streaming[A]]) => Streaming[B]): Streaming[B] =
    this match {
      case Empty() => bs
      case Next(lt) => Next(lt.map(_.foldStreaming(bs, f)))
      case This(a, lt) => f(a, lt)
    }

  /**
   * Deconstruct a stream into a head and tail (if available).
   *
   * This method will evaluate the stream until it finds a head and
   * tail, or until the stream is exhausted.
   */
  def uncons: Option[(A, Eval[Streaming[A]])] = {
    @tailrec def unroll(s: Streaming[A]): Option[(A, Eval[Streaming[A]])] =
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
  def map[B](f: A => B): Streaming[B] =
    this match {
      case Empty() => Empty()
      case Next(lt) => Next(lt.map(_.map(f)))
      case This(a, lt) => This(f(a), lt.map(_.map(f)))
    }

  /**
   * Lazily transform the stream given a function `f`.
   */
  def flatMap[B](f: A => Streaming[B]): Streaming[B] =
    this match {
      case Empty() => Empty()
      case Next(lt) => Next(lt.map(_.flatMap(f)))
      case This(a, lt) => f(a) concat lt.map(_.flatMap(f))
    }

  /**
   * Lazily filter the stream given the predicate `f`.
   */
  def filter(f: A => Boolean): Streaming[A] =
    this match {
      case Empty() =>
        this
      case Next(lt) =>
        Next(lt.map(_.filter(f)))
      case This(a, lt) =>
        val ft = lt.map(_.filter(f))
        if (f(a)) This(a, ft) else Next(ft)
    }

  /**
   * Eagerly fold the stream to a single value from the left.
   */
  def foldLeft[B](b: B)(f: (B, A) => B): B = {
    @tailrec def unroll(s: Streaming[A], b: B): B =
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
    @tailrec def unroll(s: Streaming[A]): Boolean =
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
  def concat(rhs: Streaming[A]): Streaming[A] =
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
  def concat(rhs: Eval[Streaming[A]]): Streaming[A] =
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
  def zip[B](rhs: Streaming[B]): Streaming[(A, B)] =
    (lhs.uncons, rhs.uncons) match {
      case (Some((a, lta)), Some((b, ltb))) =>
        This((a, b), Always(lta.value zip ltb.value))
      case _ =>
        Empty()
    }

  def zipWithIndex: Streaming[(A, Int)] = {
    def loop(s: Streaming[A], i: Int): Streaming[(A, Int)] =
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
  def izip[B](rhs: Streaming[B]): Streaming[Ior[A, B]] =
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
  def unzip[B, C](implicit ev: A =:= (B, C)): (Streaming[B], Streaming[C]) =
    (this.map(_._1), this.map(_._2))

  /**
   * Merge two sorted streams into a new stream.
   *
   * The streams are assumed to already be sorted. If they are not,
   * the resulting order is not defined.
   */
  def merge(rhs: Streaming[A])(implicit ev: Order[A]): Streaming[A] =
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
  def interleave(rhs: Streaming[A]): Streaming[A] =
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
  def product[B](rhs: Streaming[B]): Streaming[(A, B)] = {
    def loop(i: Int): Streaming[(A, B)] = {
      val xs = lhs.take(i + 1).asInstanceOf[Streaming[AnyRef]].toArray
      val ys = rhs.take(i + 1).asInstanceOf[Streaming[AnyRef]].toArray
      def build(j: Int): Streaming[(A, B)] =
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
    @tailrec def unroll(s: Streaming[A]): Boolean =
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
    @tailrec def unroll(s: Streaming[A]): Boolean =
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
  def take(n: Int): Streaming[A] =
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
  def drop(n: Int): Streaming[A] =
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
  def takeWhile(f: A => Boolean): Streaming[A] =
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
  def dropWhile(f: A => Boolean): Streaming[A] =
    this match {
      case Empty() => Empty()
      case Next(lt) => Next(lt.map(_.dropWhile(f)))
      case This(a, lt) => if (f(a)) Empty() else This(a, lt.map(_.takeWhile(f)))
    }

  /**
   * Provide a stream of all the tails of a stream (including itself).
   *
   * For example, Stream(1, 2).tails is equivalent to:
   *
   *   Stream(Stream(1, 2), Stream(1), Stream.empty)
   */
  def tails: Streaming[Streaming[A]] =
    uncons match {
      case None => This(this, Always(Streaming.empty))
      case Some((_, tail)) => This(this, tail.map(_.tails))
    }

  /**
   * Provide an iterator over the elements in the stream.
   */
  def iterator: Iterator[A] =
    new Iterator[A] {
      var ls: Eval[Streaming[A]] = null
      var s: Streaming[A] = lhs
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
    @tailrec def unroll(buf: mutable.ListBuffer[A], s: Streaming[A]): List[A] =
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
    @tailrec def unroll(n: Int, sb: StringBuffer, s: Streaming[A]): String =
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
    @tailrec def unroll(buf: mutable.ArrayBuffer[A], s: Streaming[A]): Array[A] =
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
  def memoize: Streaming[A] =
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
  def compact: Streaming[A] = {
    @tailrec def unroll(s: Streaming[A]): Streaming[A] =
      s match {
        case Next(lt) => unroll(lt.value)
        case s => s
      }
    unroll(this)
  }
}

object Streaming extends StreamingInstances {

  /**
   * Concrete Streaming[A] types:
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
  case class Empty[A]() extends Streaming[A]
  case class Next[A](next: Eval[Streaming[A]]) extends Streaming[A]
  case class This[A](a: A, tail: Eval[Streaming[A]]) extends Streaming[A]

  /**
   * Create an empty stream of type A.
   */
  def empty[A]: Streaming[A] =
    Empty()

  /**
   * Create a stream consisting of a single value.
   */
  def apply[A](a: A): Streaming[A] =
    This(a, Now(Empty()))

  /**
   * Create a stream from two or more values.
   */
  def apply[A](a1: A, a2: A, as: A*): Streaming[A] =
    This(a1, Now(This(a2, Now(Streaming.fromVector(as.toVector)))))

  /**
   * Defer stream creation.
   *
   * Given an expression which creates a stream, this method defers
   * that creation, allowing the head (if any) to be lazy.
   */
  def defer[A](s: => Streaming[A]): Streaming[A] =
    Next(Always(s))

  /**
   * Create a stream from a vector.
   *
   * The stream will be eagerly evaluated.
   */
  def fromVector[A](as: Vector[A]): Streaming[A] = {
    def loop(s: Streaming[A], i: Int): Streaming[A] =
      if (i < 0) s else loop(This(as(i), Now(s)), i - 1)
    loop(Empty(), as.length - 1)
  }

  /**
   * Create a stream from a vector.
   *
   * The stream will be eagerly evaluated.
   */
  def fromList[A](as: List[A]): Streaming[A] = {
    def loop(s: Streaming[A], ras: List[A]): Streaming[A] =
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
  def fromIterable[A](as: Iterable[A]): Streaming[A] =
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
  def fromIteratorUnsafe[A](it: Iterator[A]): Streaming[A] =
    if (it.hasNext) This(it.next, Later(fromIteratorUnsafe(it))) else Empty()

  /**
   * Create a self-referential stream.
   */
  def knot[A](f: Eval[Streaming[A]] => Streaming[A], memo: Boolean = false): Streaming[A] = {
    lazy val s: Eval[Streaming[A]] = if (memo) Later(f(s)) else Always(f(s))
    s.value
  }

  /**
   * Continually return a constant value.
   */
  def continually[A](a: A): Streaming[A] =
    knot(s => This(a, s))

  /**
   * Continually return the result of a thunk.
   *
   * This method only differs from `continually` in that the thunk may
   * not be pure. For this reason (and unlike continually), this
   * stream is memoized to ensure that repeated traversals produce the
   * same results.
   */
  def thunk[A](f: () => A): Streaming[A] =
    knot(s => This(f(), s), memo = true)

  /**
   * Produce an infinite stream of values given an initial value and a
   * tranformation function.
   */
  def infinite[A](a: A)(f: A => A): Streaming[A] =
    This(a, Always(infinite(f(a))(f)))

  /**
   * Stream of integers starting at n.
   */
  def from(n: Int): Streaming[Int] =
    infinite(n)(_ + 1)

  /**
   * Provide a stream of integers starting with `start` and ending
   * with `end` (i.e. inclusive).
   */
  def interval(start: Int, end: Int): Streaming[Int] =
    if (start > end) Empty() else This(start, Always(interval(start + 1, end)))

  /**
   * Produce a stream given an "unfolding" function.
   *
   * None represents an empty stream. Some(a) reprsents an initial
   * element, and we can compute the tail (if any) via f(a).
   */
  def unfold[A](o: Option[A])(f: A => Option[A]): Streaming[A] =
    o match {
      case None => Empty()
      case Some(a) => This(a, Always(unfold(f(a))(f)))
    }

  /**
   * An empty loop, will wait forever if evaluated.
   */
  def godot: Streaming[Nothing] =
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
      def unapply[A](s: Streaming[A]): Option[(A, Eval[Streaming[A]])] = s.uncons
    }

    class StreamOps[A](rhs: Eval[Streaming[A]]) {
      def %::(lhs: A): Streaming[A] = This(lhs, rhs)
      def %:::(lhs: Streaming[A]): Streaming[A] = lhs concat rhs
    }

    implicit def streamOps[A](as: => Streaming[A]): StreamOps[A] =
      new StreamOps(Always(as))
  }
}

trait StreamingInstances {

  implicit val streamInstance: Traverse[Streaming] with MonadCombine[Streaming] with CoflatMap[Streaming] =
    new Traverse[Streaming] with MonadCombine[Streaming] with CoflatMap[Streaming] {
      def pure[A](a: A): Streaming[A] =
        Streaming(a)
      override def map[A, B](as: Streaming[A])(f: A => B): Streaming[B] =
        as.map(f)
      def flatMap[A, B](as: Streaming[A])(f: A => Streaming[B]): Streaming[B] =
        as.flatMap(f)
      def empty[A]: Streaming[A] =
        Streaming.empty
      def combine[A](xs: Streaming[A], ys: Streaming[A]): Streaming[A] =
        xs concat ys

      override def map2[A, B, Z](fa: Streaming[A], fb: Streaming[B])(f: (A, B) => Z): Streaming[Z] =
        fa.flatMap(a => fb.map(b => f(a, b)))

      def coflatMap[A, B](fa: Streaming[A])(f: Streaming[A] => B): Streaming[B] =
        fa.tails.filter(_.nonEmpty).map(f)

      def foldLeft[A, B](fa: Streaming[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: Streaming[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)

      def traverse[G[_]: Applicative, A, B](fa: Streaming[A])(f: A => G[B]): G[Streaming[B]] = {
        val G = Applicative[G]
        def init: G[Streaming[B]] = G.pure(Streaming.empty[B])

        // We use foldRight to avoid possible stack overflows. Since
        // we don't want to return a Eval[_] instance, we call .value
        // at the end.
        //
        // (We don't worry about internal laziness because traverse
        // has to evaluate the entire stream anyway.)
        import Streaming.syntax._
        foldRight(fa, Later(init)) { (a, lgsb) =>
          lgsb.map(gsb => G.map2(f(a), gsb)(_ %:: _))
        }.value
      }

      override def exists[A](fa: Streaming[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: Streaming[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def isEmpty[A](fa: Streaming[A]): Boolean =
        fa.isEmpty
    }

  import Streaming.{Empty, Next, This}

  implicit def streamEq[A: Eq]: Eq[Streaming[A]] =
    new Eq[Streaming[A]] {
      def eqv(x: Streaming[A], y: Streaming[A]): Boolean = {
        @tailrec def loop(x: Streaming[A], y: Streaming[A]): Boolean =
          x match {
            case Empty() => y.isEmpty
            case Next(lt1) => loop(lt1.value, y)
            case This(a1, lt1) =>
              y match {
                case Empty() => false
                case Next(lt2) => loop(x, lt2.value)
                case This(a2, lt2) => if (a1 =!= a2) false else loop(lt1.value, lt2.value)
              }
          }
        loop(x, y)
      }
    }
}
