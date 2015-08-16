package cats
package data

import cats.syntax.flatMap._
import cats.syntax.functor._
import scala.reflect.ClassTag

import scala.annotation.tailrec
import scala.collection.mutable

sealed abstract class StreamT[F[_], A] { lhs =>

  import StreamT.{Empty, Next, This}

  /**
   * Deconstruct a stream into a head and tail (if available).
   *
   * This method will evaluate the stream until it finds a head and
   * tail, or until the stream is exhausted.
   */
  def uncons(implicit ev: Monad[F]): F[Option[(A, StreamT[F, A])]] =
    this match {
      case Empty() => ev.pure(None)
      case Next(ft) => ft.flatMap(_.uncons)
      case This(a, ft) => ft.map(t => Some((a, t)))
    }

  /**
   * Lazily transform the stream given a function `f`.
   */
  def map[B](f: A => B)(implicit ev: Functor[F]): StreamT[F, B] =
    this match {
      case Empty() => Empty()
      case Next(ft) => Next(ft.map(_.map(f)))
      case This(a, ft) => This(f(a), ft.map(_.map(f)))
    }

  /**
   * Lazily transform the stream given a function `f`.
   */
  def flatMap[B](f: A => StreamT[F, B])(implicit ev: Functor[F]): StreamT[F, B] =
    this match {
      case Empty() => Empty()
      case Next(ft) => Next(ft.map(_.flatMap(f)))
      case This(a, ft) => f(a) concat ft.map(_.flatMap(f))
    }

  /**
   * Lazily filter the stream given the predicate `f`.
   */
  def filter(f: A => Boolean)(implicit ev: Functor[F]): StreamT[F, A] =
    this match {
      case Empty() => this
      case Next(ft) => Next(ft.map(_.filter(f)))
      case This(a, ft) => if (f(a)) this else Next(ft.map(_.filter(f)))
    }

  /**
   * Eagerly fold the stream to a single value from the left.
   */
  def foldLeft[B](b: B)(f: (B, A) => B)(implicit ev: Monad[F]): F[B] =
    this match {
      case Empty() => ev.pure(b)
      case Next(ft) => ft.flatMap(_.foldLeft(b)(f))
      case This(a, ft) => ft.flatMap(_.foldLeft(f(b, a))(f))
    }

  /**
   * Return true if the stream is empty, false otherwise.
   *
   * In this case of deferred streams this will force the first
   * element to be calculated.
   */
  def isEmpty(implicit ev: Monad[F]): F[Boolean] =
    uncons.map(_.isDefined)

  /**
   * Return true if the stream is non-empty, false otherwise.
   *
   * In this case of deferred streams this will force the first
   * element to be calculated.
   */
  def nonEmpty(implicit ev: Monad[F]): F[Boolean] =
    uncons.map(_.isEmpty)

  /**
   * Lazily concatenate two streams.
   */
  def concat(rhs: StreamT[F, A])(implicit ev: Functor[F]): StreamT[F, A] =
    this match {
      case Empty() => rhs
      case Next(ft) => Next(ft.map(_ concat rhs))
      case This(a, ft) => This(a, ft.map(_ concat rhs))
    }

  /**
   * Lazily concatenate two streams.
   *
   * In this case the evaluation of the second stream may be deferred.
   */
  def concat(rhs: F[StreamT[F, A]])(implicit ev: Functor[F]): StreamT[F, A] =
    this match {
      case Empty() => Next(rhs)
      case Next(ft) => Next(ft.map(_ concat rhs))
      case This(a, ft) => This(a, ft.map(_ concat rhs))
    }

  /**
   * Lazily zip two streams together.
   *
   * The lenght of the result will be the shorter of the two
   * arguments.
   */
  def zip[B](rhs: StreamT[F, B])(implicit ev: Monad[F]): StreamT[F, (A, B)] =
    Next(for {
      lo <- lhs.uncons; ro <- rhs.uncons
    } yield (lo, ro) match {
      case (Some((a, ta)), Some((b, tb))) =>
        This((a, b), ev.pure(ta zip tb))
      case _ =>
        Empty()
    })

  /**
   * Lazily zip two streams together using Ior.
   *
   * Unlike `zip`, the length of the result will be the longer of the
   * two arguments.
   */
  def izip[B](rhs: StreamT[F, B])(implicit ev: Monad[F]): StreamT[F, Ior[A, B]] =
    Next(for {
      lo <- lhs.uncons; ro <- rhs.uncons
    } yield (lo, ro) match {
      case (Some((a, ta)), Some((b, tb))) =>
        This(Ior.both(a, b), ev.pure(ta izip tb))
      case (Some(_), None) =>
        lhs.map(a => Ior.left(a))
      case (None, Some(_)) =>
        rhs.map(b => Ior.right(b))
      case _ =>
        Empty()
    })

  /**
   * Return true if some element of the stream satisfies the
   * predicate, false otherwise.
   */
  def exists(f: A => Boolean)(implicit ev: Monad[F]): F[Boolean] =
    this match {
      case Empty() => ev.pure(false)
      case Next(ft) => ft.flatMap(_.exists(f))
      case This(a, ft) => if (f(a)) ev.pure(true) else ft.flatMap(_.exists(f))
    }

  /**
   * Return true if every element of the stream satisfies the
   * predicate, false otherwise.
   */
  def forall(f: A => Boolean)(implicit ev: Monad[F]): F[Boolean] =
    this match {
      case Empty() => ev.pure(true)
      case Next(ft) => ft.flatMap(_.exists(f))
      case This(a, ft) => if (!f(a)) ev.pure(false) else ft.flatMap(_.forall(f))
    }

  /**
   * Return a stream consisting only of the first `n` elements of this
   * stream.
   *
   * If the current stream has `n` or fewer elements, the entire
   * stream will be returned.
   */
  def take(n: Int)(implicit ev: Functor[F]): StreamT[F, A] =
    if (n <= 0) Empty() else this match {
      case Empty() => Empty()
      case Next(ft) => Next(ft.map(_.take(n)))
      case This(a, ft) => This(a, ft.map(_.take(n - 1)))
    }

  /**
   * Return a stream consisting of all but the first `n` elements of
   * this stream.
   *
   * If the current stream has `n` or fewer elements, an empty stream
   * will be returned.
   */
  def drop(n: Int)(implicit ev: Functor[F]): StreamT[F, A] =
    if (n <= 0) this else this match {
      case Empty() => Empty()
      case Next(ft) => Next(ft.map(_.drop(n)))
      case This(a, ft) => Next(ft.map(_.take(n - 1)))
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
  def takeWhile(f: A => Boolean)(implicit ev: Functor[F]): StreamT[F, A] =
    this match {
      case Empty() => Empty()
      case Next(ft) => Next(ft.map(_.takeWhile(f)))
      case This(a, ft) => if (f(a)) This(a, ft.map(_.takeWhile(f))) else Empty()
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
  def dropWhile(f: A => Boolean)(implicit ev: Functor[F]): StreamT[F, A] =
    this match {
      case Empty() => Empty()
      case Next(ft) => Next(ft.map(_.dropWhile(f)))
      case This(a, ft) => if (f(a)) Empty() else This(a, ft.map(_.takeWhile(f)))
    }

  /**
   * Provide a list of elements in the stream.
   *
   * This will evaluate the stream immediately, and will hang in the
   * case of infinite streams.
   */
  def toList(implicit ev: Monad[F]): F[List[A]] =
    this match {
      case Empty() => ev.pure(Nil)
      case Next(ft) => ft.flatMap(_.toList)
      case This(a, ft) => ft.flatMap(_.toList).map(a :: _)
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
}

object StreamT {

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
  case class Empty[F[_], A]() extends StreamT[F, A]
  case class Next[F[_], A](next: F[StreamT[F, A]]) extends StreamT[F, A]
  case class This[F[_], A](a: A, tail: F[StreamT[F, A]]) extends StreamT[F, A]

  /**
   * Create an empty stream of type A.
   */
  def empty[F[_], A]: StreamT[F, A] =
    Empty()

  /**
   * Create a stream consisting of a single value.
   */
  def apply[F[_], A](a: A)(implicit ev: Applicative[F]): StreamT[F, A] =
    This(a, ev.pure(Empty()))

  def cons[F[_], A](a: A, fs: F[StreamT[F, A]]): StreamT[F, A] =
    This(a, fs)

  /**
   * Produce a stream given an "unfolding" function.
   *
   * None represents an empty stream. Some(a) reprsents an initial
   * element, and we can compute the tail (if any) via f(a).
   */
  def unfold[F[_], A](o: Option[A])(f: A => F[Option[A]])(implicit ev: Functor[F]): StreamT[F, A] =
    o match {
      case None => Empty()
      case Some(a) => This(a, f(a).map(o => unfold(o)(f)))
    }
}
