package cats
package data

import cats.syntax.all._

import scala.reflect.ClassTag
import scala.annotation.tailrec
import scala.collection.mutable

/**
 * `Streaming[A]` represents a stream of values. A stream can be
 * thought of as a collection, with two key differences:
 *
 *  1. It may be infinite; it does not necessarily have a finite
 *     length. For this reason, there is no `.length` method.
 *
 *  2. It may be lazy. In other words, the entire stream may not be in
 *     memory. In this case, each "step" of the stream has
 *     instructions for producing the next step.
 *
 * Streams are not necessarily lazy: they use `Eval[Streaming[A]]` to
 * represent a tail that may (or may not be) lazy. If `Now[A]` is used
 * for each tail, then `Streaming[A]` will behave similarly to
 * `List[A]`. If `Later[A]` is used for each tail, then `Streaming[A]`
 * will behave similarly to `scala.Stream[A]` (i.e. it will
 * lazily-compute the tail, and will memoize the result to improve the
 * performance of repeated traversals). If `Always[A]` is used for
 * each tail, the result will be a lazy stream which does not memoize
 * results (saving space at the cost of potentially-repeated
 * calculations).
 *
 * Since `Streaming[A]` has been compared to `scala.Stream[A]` it is
 * worth noting some key differences between the two types:
 *
 *  1. When the entire stream is known ahead of time, `Streaming[A]`
 *     can represent it more efficiencly, using `Now[A]`, rather than
 *     allocating a list of closures.
 *
 *  2. `Streaming[A]` does not memoize by default. This protects
 *     against cases where a reference to head will prevent the entire
 *     stream from being garbage collected, and is a better default.
 *     A stream can be memoized later using the `.memoize` method.
 *
 *  3. `Streaming[A]` does not inherit from the standard collections,
 *     meaning a wide variety of methods which are dangerous on
 *     streams (`.length`, `.apply`, etc.) are not present.
 *
 *  4. `scala.Stream[A]` requires an immediate value for `.head`. This
 *     means that operations like `.filter` will block until a
 *     matching value is found, or the stream is exhausted (which
 *     could be never in the case of an infinite stream). By contrast,
 *     `Streaming[A]` values can be totally lazy (and can be
 *     lazily-constructed using `Streaming.defer()`), so methods like
 *     `.filter` are completely lazy.
 *
 *  5. The use of `Eval[Streaming[A]]` to represent the "tail" of the
 *     stream means that streams can be lazily (and safely)
 *     constructed with `Foldable#foldRight`, and that `.map` and
 *     `.flatMap` operations over the tail will be safely trampolined.
 */
sealed abstract class Streaming[A] extends Product with Serializable { lhs =>

  import Streaming.{Empty, Wait, Cons}

  /**
   * The stream's catamorphism.
   *
   * This method allows the stream to be transformed into an arbitrary
   * value by handling two cases:
   *
   *  1. empty stream: return b
   *  2. non-empty stream: apply the function to the head and tail
   *
   * This method can be more convenient than pattern-matching, since
   * it includes support for handling deferred streams (i.e. Wait(_)),
   * these nodes will be evaluated until an empty or non-empty stream
   * is found (i.e. until Empty() or Cons() is found).
   */
  def fold[B](b: Eval[B], f: (A, Eval[Streaming[A]]) => B): B = {
    @tailrec def unroll(s: Streaming[A]): B =
      s match {
        case Empty() => b.value
        case Wait(lt) => unroll(lt.value)
        case Cons(a, lt) => f(a, lt)
      }
    unroll(this)
  }

  /**
   * A variant of fold, used for constructing streams.
   *
   * The only difference is that foldStreaming will preserve deferred
   * streams. This makes it more appropriate to use in situations
   * where the stream's laziness must be preserved.
   */
  def foldStreaming[B](bs: => Streaming[B], f: (A, Eval[Streaming[A]]) => Streaming[B]): Streaming[B] =
    this match {
      case Empty() => bs
      case Wait(lt) => Wait(lt.map(_.foldStreaming(bs, f)))
      case Cons(a, lt) => f(a, lt)
    }

  /**
   * Deconstruct a stream into a head and tail (if available).
   *
   * This method will evaluate the stream until it finds a head and
   * tail, or until the stream is exhausted. The head will be
   * evaluated, whereas the tail will remain (potentially) lazy within
   * Eval.
   */
  def uncons: Option[(A, Eval[Streaming[A]])] = {
    @tailrec def unroll(s: Streaming[A]): Option[(A, Eval[Streaming[A]])] =
      s match {
        case Empty() => None
        case Wait(lt) => unroll(lt.value)
        case Cons(a, lt) => Some((a, lt))
      }
    unroll(this)
  }

  /**
   * Lazily transform the stream given a function `f`.
   */
  def map[B](f: A => B): Streaming[B] =
    this match {
      case Empty() => Empty()
      case Wait(lt) => Wait(lt.map(_.map(f)))
      case Cons(a, lt) => Cons(f(a), lt.map(_.map(f)))
    }

  /**
   * Lazily transform the stream given a function `f`.
   */
  def flatMap[B](f: A => Streaming[B]): Streaming[B] =
    this match {
      case Empty() => Empty()
      case Wait(lt) => Wait(lt.map(_.flatMap(f)))
      case Cons(a, lt) => f(a) ++ lt.map(_.flatMap(f))
    }

  /**
   * Lazily filter the stream given the predicate `f`.
   */
  def filter(f: A => Boolean): Streaming[A] =
    this match {
      case Empty() =>
        this
      case Wait(lt) =>
        Wait(lt.map(_.filter(f)))
      case Cons(a, lt) =>
        val ft = lt.map(_.filter(f))
        if (f(a)) Cons(a, ft) else Wait(ft)
    }

  /**
   * Eagerly fold the stream to a single value from the left.
   */
  def foldLeft[B](b: B)(f: (B, A) => B): B = {
    @tailrec def unroll(s: Streaming[A], b: B): B =
      s match {
        case Empty() => b
        case Wait(lt) => unroll(lt.value, b)
        case Cons(a, lt) => unroll(lt.value, f(b, a))
      }
    unroll(this, b)
  }

  /**
   * Lazily fold the stream to a single value from the right.
   */
  def foldRight[B](b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    this match {
      case Empty() => b
      case Wait(lt) => lt.flatMap(_.foldRight(b)(f))
      case Cons(a, lt) => f(a, lt.flatMap(_.foldRight(b)(f)))
    }

  /**
   * Eagerly search the stream from the left. The search ends when f
   * returns true for some a, or the stream is exhausted. Some(a)
   * signals a match, None means no matching items were found.
   */
  def find(f: A => Boolean): Option[A] = {
    @tailrec def loop(s: Streaming[A]): Option[A] =
      s match {
        case Cons(a, lt) => if (f(a)) Some(a) else loop(lt.value)
        case Wait(lt) => loop(lt.value)
        case Empty() => None
      }
    loop(this)
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
        case Cons(_, _) => false
        case Empty() => true
        case Wait(lt) => unroll(lt.value)
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
      case Wait(_) => None
      case Cons(a, lt) => Some(false)
    }

  /**
   * Lazily concatenate two streams.
   */
  def ++(rhs: Streaming[A]): Streaming[A] =
    this match {
      case Empty() => rhs
      case Wait(lt) => Wait(lt.map(_ ++ rhs))
      case Cons(a, lt) => Cons(a, lt.map(_ ++ rhs))
    }

  /**
   * Lazily concatenate two streams.
   *
   * In this case the evaluation of the second stream may be deferred.
   */
  def ++(rhs: Eval[Streaming[A]]): Streaming[A] =
    this match {
      case Empty() => Wait(rhs)
      case Wait(lt) => Wait(lt.map(_ ++ rhs))
      case Cons(a, lt) => Cons(a, lt.map(_ ++ rhs))
    }

  /**
   * Lazily zip two streams together.
   *
   * The length of the result will be the shorter of the two
   * arguments.
   */
  def zip[B](rhs: Streaming[B]): Streaming[(A, B)] =
    (lhs zipMap rhs)((a, b) => (a, b))

  /**
   * Lazily zip two streams together, using the given function `f` to
   * produce output values.
   *
   * The length of the result will be the shorter of the two
   * arguments.
   *
   * The expression:
   *
   *   (lhs zipMap rhs)(f)
   *
   * is equivalent to (but more efficient than):
   *
   *   (lhs zip rhs).map { case (a, b) => f(a, b) }
   */
  def zipMap[B, C](rhs: Streaming[B])(f: (A, B) => C): Streaming[C] =
    (lhs, rhs) match {
      case (Cons(a, lta), Cons(b, ltb)) =>
        Cons(f(a, b), for { ta <- lta; tb <- ltb } yield (ta zipMap tb)(f))
      case (Empty(), _) =>
        Empty()
      case (_, Empty()) =>
        Empty()
      case (Wait(lta), s) =>
        Wait(lta.map(_.zipMap(s)(f)))
      case (s, Wait(ltb)) =>
        Wait(ltb.map(s.zipMap(_)(f)))
    }

  /**
   * Lazily zip two streams together using Ior.
   *
   * Unlike `zip`, the length of the result will be the longer of the
   * two arguments.
   */
  def izip[B](rhs: Streaming[B]): Streaming[Ior[A, B]] =
    izipMap(rhs)(Ior.both, Ior.left, Ior.right)

  /**
   * Zip two streams together, using the given function `f` to produce
   * the output values.
   *
   * Unlike zipMap, the length of the result will be the *longer* of
   * the two input streams. The functions `g` and `h` will be used in
   * this case to produce valid `C` values.
   *
   * The expression:
   *
   *   (lhs izipMap rhs)(f, g, h)
   *
   * is equivalent to (but more efficient than):
   *
   *   (lhs izip rhs).map {
   *     case Ior.Both(a, b) => f(a, b)
   *     case Ior.Left(a) => g(a)
   *     case Ior.Right(b) => h(b)
   *   }
   */
  def izipMap[B, C](rhs: Streaming[B])(f: (A, B) => C, g: A => C, h: B => C): Streaming[C] =
    (lhs, rhs) match {
      case (Cons(a, lta), Cons(b, ltb)) =>
        Cons(f(a, b), for { ta <- lta; tb <- ltb } yield (ta izipMap tb)(f, g, h))
      case (Wait(lta), tb) =>
        Wait(lta.map(_.izipMap(tb)(f, g, h)))
      case (ta, Wait(ltb)) =>
        Wait(ltb.map(ta.izipMap(_)(f, g, h)))
      case (Empty(), tb) =>
        tb.map(h)
      case (ta, Empty()) =>
        ta.map(g)
    }

  /**
   * Zip the items of the stream with their position.
   *
   * Indices start at 0, so
   *
   *   Streaming('x, 'y, 'z).zipWithIndex
   *
   * lazily produces:
   *
   *   Streaming(('x, 0), ('y, 1), ('z, 2))
   */
  def zipWithIndex: Streaming[(A, Int)] = {
    def loop(s: Streaming[A], i: Int): Streaming[(A, Int)] =
      s match {
        case Empty() => Empty()
        case Wait(lt) => Wait(lt.map(s => loop(s, i)))
        case Cons(a, lt) => Cons((a, i), lt.map(s => loop(s, i + 1)))
      }
    loop(this, 0)
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
    (lhs, rhs) match {
      case (Cons(a0, lt0), Cons(a1, lt1)) =>
        if (a0 < a1) Cons(a0, lt0.map(_ merge rhs)) else Cons(a1, lt1.map(lhs merge _))
      case (Wait(lta), s) =>
        Wait(lta.map(_ merge s))
      case (s, Wait(ltb)) =>
        Wait(ltb.map(s merge _))
      case (s, Empty()) =>
        s
      case (Empty(), s) =>
        s
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
    lhs match {
      case Cons(a, lt) => Cons(a, lt.map(rhs interleave _))
      case Wait(lt) => Wait(lt.map(_ interleave rhs))
      case Empty() => rhs
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
   *
   * This method lazily evaluates the input streams, but due to the
   * diagonalization method may read ahead more than is strictly
   * necessary.
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
            Cons(tpl, Always(build(j + 1)))
          }
        }
      if (i > xs.length + ys.length - 2) Empty() else {
        build(0) ++ Always(loop(i + 1))
      }
    }
    Wait(Always(loop(0)))
  }

  /**
   * Return true if some element of the stream satisfies the
   * predicate, false otherwise.
   */
  def exists(f: A => Boolean): Boolean = {
    @tailrec def unroll(s: Streaming[A]): Boolean =
      s match {
        case Empty() => false
        case Wait(lt) => unroll(lt.value)
        case Cons(a, lt) => if (f(a)) true else unroll(lt.value)
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
        case Wait(lt) => unroll(lt.value)
        case Cons(a, lt) => if (f(a)) unroll(lt.value) else false
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
      case Wait(lt) => Wait(lt.map(_.take(n)))
      case Cons(a, lt) =>
        Cons(a, if (n == 1) Now(Empty()) else lt.map(_.take(n - 1)))
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
      case Wait(lt) => Wait(lt.map(_.drop(n)))
      case Cons(a, lt) => Wait(lt.map(_.drop(n - 1)))
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
   * {{{
   * scala> val s = Streaming(1, 2, 3, 4, 5, 6, 7)
   * scala> s.takeWhile(n => n != 4).toList
   * res0: List[Int] = List(1, 2, 3)
   * }}}
   */
  def takeWhile(f: A => Boolean): Streaming[A] =
    this match {
      case Empty() => Empty()
      case Wait(lt) => Wait(lt.map(_.takeWhile(f)))
      case Cons(a, lt) => if (f(a)) Cons(a, lt.map(_.takeWhile(f))) else Empty()
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
   * {{{
   * scala> val s = Streaming(1, 2, 3, 4, 5, 6, 7)
   * scala> s.dropWhile(n => n != 4).toList
   * res0: List[Int] = List(4, 5, 6, 7)
   * }}}
   */
  def dropWhile(f: A => Boolean): Streaming[A] =
    this match {
      case Empty() => Empty()
      case Wait(lt) => Wait(lt.map(_.dropWhile(f)))
      case s @ Cons(a, lt) => if (f(a)) Wait(lt.map(_.dropWhile(f))) else s
    }

  /**
   * Provide a stream of all the tails of a stream (including itself).
   *
   * For example, Streaming(1, 2).tails is equivalent to:
   *
   *   Streaming(Streaming(1, 2), Streaming(1), Streaming.empty)
   */
  def tails: Streaming[Streaming[A]] =
    this match {
      case Cons(a, lt) => Cons(this, lt.map(_.tails))
      case Wait(lt) => Wait(lt.map(_.tails))
      case Empty() => Cons(this, Always(Streaming.empty))
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

      val emptyCase: Eval[A] =
        Always(throw new NoSuchElementException("next on empty iterator"))
      val consCase: (A, Eval[Streaming[A]]) => A =
        (a, lt) => { ls = lt; s = null; a }

      def next: A = {
        if (s == null) s = ls.value
        s.fold(emptyCase, consCase)
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
        case Wait(lt) => unroll(buf, lt.value)
        case Cons(a, lt) => unroll(buf += a, lt.value)
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
      case Cons(a, _) => "Streaming(" + a + ", ...)"
      case Empty() => "Streaming()"
      case Wait(_) => "Streaming(...)"
    }

  /**
   * String representation of the first n elements of a stream.
   */
  def toString(limit: Int = 10): String = {
    @tailrec def unroll(n: Int, sb: StringBuffer, s: Streaming[A]): String =
      if (n <= 0) sb.append(", ...)").toString else s match {
        case Empty() => sb.append(")").toString
        case Wait(lt) => unroll(n, sb, lt.value)
        case Cons(a, lt) => unroll(n - 1, sb.append(", " + a.toString), lt.value)
      }
    uncons match {
      case None =>
        "Streaming()"
      case Some((a, lt)) =>
        val sb = new StringBuffer().append("Streaming(" + a.toString)
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
        case Wait(lt) => unroll(buf, lt.value)
        case Cons(a, lt) => unroll(buf += a, lt.value)
      }
    unroll(mutable.ArrayBuffer.empty[A], this)
  }

  /**
   * Ensure that repeated traversals of the stream will not cause
   * repeated tail computations.
   *
   * By default this structure does not memoize to avoid memory leaks
   * when the head of the stream is retained. However, the user
   * ultimately has control of the memoization approach based on what
   * kinds of Eval instances they use.
   *
   * There are two calls to .memoize here -- one is a recursive call
   * to this method (on the tail) and the other is a call to memoize
   * the Eval instance holding the tail. For more information on how
   * this works see [[cats.Eval.memoize]].
   */
  def memoize: Streaming[A] =
    this match {
      case Empty() => Empty()
      case Wait(lt) => Wait(lt.map(_.memoize).memoize)
      case Cons(a, lt) => Cons(a, lt.map(_.memoize).memoize)
    }

  /**
   * Compact removes "pauses" in the stream (represented as Wait(_)
   * nodes).
   *
   * Normally, Wait(_) values are used to defer tail computation in
   * cases where it is convenient to return a stream value where
   * neither the head or tail are computed yet.
   *
   * In some cases (particularly if the stream is to be memoized) it
   * may be desirable to ensure that these values are not retained.
   */
  def compact: Streaming[A] = {
    @tailrec def unroll(s: Streaming[A]): Streaming[A] =
      s match {
        case Cons(a, lt) => Cons(a, lt.map(_.compact))
        case Wait(lt) => unroll(lt.value)
        case Empty() => Empty()
      }
    unroll(this)
  }
}

object Streaming extends StreamingInstances {

  /**
   * Concrete Streaming[A] types:
   *
   *  - Empty(): an empty stream.
   *  - Cons(a, tail): a non-empty stream containing (at least) `a`.
   *  - Wait(tail): a deferred stream.
   *
   * Cons represents a lazy, possibly infinite stream of values.
   * Eval[_] is used to represent possible laziness (via Now, Later,
   * and Always). The head of `Cons` is eager -- a lazy head can be
   * represented using `Wait(Always(...))` or `Wait(Later(...))`.
   */
  final case class Empty[A]() extends Streaming[A]
  final case class Wait[A](next: Eval[Streaming[A]]) extends Streaming[A]
  final case class Cons[A](a: A, tail: Eval[Streaming[A]]) extends Streaming[A]

  /**
   * Create an empty stream of type A.
   */
  def empty[A]: Streaming[A] =
    Empty()

  /**
   * Create a stream consisting of a single value.
   */
  def apply[A](a: A): Streaming[A] =
    Cons(a, Now(Empty()))

  /**
   * Prepend a value to a stream.
   */
  def cons[A](a: A, s: Streaming[A]): Streaming[A] =
    Cons(a, Now(s))

  /**
   * Prepend a value to an Eval[Streaming[A]].
   */
  def cons[A](a: A, ls: Eval[Streaming[A]]): Streaming[A] =
    Cons(a, ls)

  /**
   * Create a stream from two or more values.
   */
  def apply[A](a1: A, a2: A, as: A*): Streaming[A] =
    cons(a1, cons(a2, fromVector(as.toVector)))

  /**
   * Defer stream creation.
   *
   * Given an expression which creates a stream, this method defers
   * that creation, allowing the head (if any) to be lazy.
   */
  def defer[A](s: => Streaming[A]): Streaming[A] =
    wait(Always(s))

  /**
   * Create a stream from an `Eval[Streaming[A]]` value.
   *
   * Given an expression which creates a stream, this method defers
   * that creation, allowing the head (if any) to be lazy.
   */
  def wait[A](ls: Eval[Streaming[A]]): Streaming[A] =
    Wait(ls)

  /**
   * Create a stream from a vector.
   *
   * The stream will be eagerly evaluated.
   */
  def fromVector[A](as: Vector[A]): Streaming[A] = {
    def loop(s: Streaming[A], i: Int): Streaming[A] =
      if (i < 0) s else loop(Cons(as(i), Now(s)), i - 1)
    loop(Empty(), as.length - 1)
  }

  /**
   * Create a stream from a list.
   *
   * The stream will be eagerly evaluated.
   */
  def fromList[A](as: List[A]): Streaming[A] = {
    def loop(s: Streaming[A], ras: List[A]): Streaming[A] =
      ras match {
        case Nil => s
        case a :: rt => loop(Cons(a, Now(s)), rt)
      }
    loop(Empty(), as.reverse)
  }

  def fromFoldable[F[_], A](fa: F[A])(implicit F: Foldable[F]): Streaming[A] =
    F.toStreaming(fa)

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
    if (it.hasNext) Cons(it.next, Later(fromIteratorUnsafe(it))) else Empty()

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
    knot(s => Cons(a, s), memo = true)

  /**
   * Continually return the result of a thunk.
   *
   * This method only differs from `continually` in that the thunk may
   * not be pure. Thus, repeated traversals may produce different
   * results.
   */
  def thunk[A](f: () => A): Streaming[A] =
    knot(s => Cons(f(), s), memo = false)

  /**
   * Produce an infinite stream of values given an initial value and a
   * tranformation function.
   */
  def infinite[A](a: A)(f: A => A): Streaming[A] =
    Cons(a, Always(infinite(f(a))(f)))

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
    if (start > end) Empty() else Cons(start, Always(interval(start + 1, end)))

  /**
   * Produce a stream given an "unfolding" function.
   *
   * None represents an empty stream. Some(a) reprsents an initial
   * element, and we can compute the tail (if any) via f(a).
   */
  def unfold[A](o: Option[A])(f: A => Option[A]): Streaming[A] =
    o match {
      case None => Empty()
      case Some(a) => Cons(a, Always(unfold(f(a))(f)))
    }
}

private[data] sealed trait StreamingInstances extends StreamingInstances1 {

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
      def combineK[A](xs: Streaming[A], ys: Streaming[A]): Streaming[A] =
        xs ++ ys

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
        foldRight(fa, Later(init)) { (a, lgsb) =>
          lgsb.map(gsb => G.map2(f(a), gsb) { (a, s) => Streaming.cons(a, s) })
        }.value
      }

      override def exists[A](fa: Streaming[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: Streaming[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def isEmpty[A](fa: Streaming[A]): Boolean =
        fa.isEmpty

      override def toStreaming[A](fa: Streaming[A]): Streaming[A] =
        fa
    }

  implicit def streamOrder[A: Order]: Order[Streaming[A]] =
    new Order[Streaming[A]] {
      def compare(x: Streaming[A], y: Streaming[A]): Int =
        (x izipMap y)(_ compare _, _ => 1, _ => -1)
          .find(_ != 0).getOrElse(0)
    }
}

private[data] sealed trait StreamingInstances1 extends StreamingInstances2 {
  implicit def streamPartialOrder[A: PartialOrder]: PartialOrder[Streaming[A]] =
    new PartialOrder[Streaming[A]] {
      def partialCompare(x: Streaming[A], y: Streaming[A]): Double =
        (x izipMap y)(_ partialCompare _, _ => 1.0, _ => -1.0)
          .find(_ != 0.0).getOrElse(0.0)
    }
}

private[data] sealed trait StreamingInstances2 {
  implicit def streamEq[A: Eq]: Eq[Streaming[A]] =
    new Eq[Streaming[A]] {
      def eqv(x: Streaming[A], y: Streaming[A]): Boolean =
        (x izipMap y)(_ === _, _ => false, _ => false)
          .forall(_ == true)
    }
}
