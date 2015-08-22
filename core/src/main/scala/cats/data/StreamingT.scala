package cats
package data

import cats.syntax.flatMap._
import cats.syntax.functor._
import scala.reflect.ClassTag

import scala.annotation.tailrec
import scala.collection.mutable

sealed abstract class StreamingT[F[_], A] { lhs =>

  import StreamingT.{Empty, Next, This}

  /**
   * Deconstruct a stream into a head and tail (if available).
   *
   * This method will evaluate the stream until it finds a head and
   * tail, or until the stream is exhausted.
   */
  def uncons(implicit ev: Monad[F]): F[Option[(A, StreamingT[F, A])]] =
    this match {
      case This(a, ft) => ft.map(t => Some((a, t)))
      case Next(ft) => ft.flatMap(_.uncons)
      case Empty() => ev.pure(None)
    }

  /**
   * Lazily transform the stream given a function `f`.
   */
  def map[B](f: A => B)(implicit ev: Functor[F]): StreamingT[F, B] =
    this match {
      case This(a, ft) => This(f(a), ft.map(_.map(f)))
      case Next(ft) => Next(ft.map(_.map(f)))
      case Empty() => Empty()
    }

  /**
   * Lazily transform the stream given a function `f`.
   */
  def flatMap[B](f: A => StreamingT[F, B])(implicit ev: Monad[F]): StreamingT[F, B] = {
    this match {
      case This(a, ft) =>
        f(a) match {
          case This(a0, ft0) => This(a0, ft0.flatMap(_ fconcat ft.map(_.flatMap(f))))
          case Next(ft0) => Next(ft0.flatMap(_ fconcat ft.map(_.flatMap(f))))
          case Empty() => Next(ft.map(_.flatMap(f)))
        }
      case Next(ft) =>
        Next(ft.map(_.flatMap(f)))
      case Empty() =>
        Empty()
    }
  }

  /**
   * Lazily filter the stream given the predicate `f`.
   */
  def filter(f: A => Boolean)(implicit ev: Functor[F]): StreamingT[F, A] =
    this match {
      case This(a, ft) => if (f(a)) this else Next(ft.map(_.filter(f)))
      case Next(ft) => Next(ft.map(_.filter(f)))
      case Empty() => this
    }

  /**
   * Eagerly fold the stream to a single value from the left.
   */
  def foldLeft[B](b: B)(f: (B, A) => B)(implicit ev: Monad[F]): F[B] =
    this match {
      case This(a, ft) => ft.flatMap(_.foldLeft(f(b, a))(f))
      case Next(ft) => ft.flatMap(_.foldLeft(b)(f))
      case Empty() => ev.pure(b)
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
   * Prepend an A value to the current stream.
   */
  def %::(a: A)(implicit ev: Applicative[F]): StreamingT[F, A] =
    This(a, ev.pure(this))

  /**
   * Prepend a StreamingT[F, A] value to the current stream.
   */
  def %:::(lhs: StreamingT[F, A])(implicit ev: Functor[F]): StreamingT[F, A] =
    lhs match {
      case This(a, ft) => This(a, ft.map(_ %::: this))
      case Next(ft) => Next(ft.map(_ %::: this))
      case Empty() => this
    }

  /**
   * Concatenate streaming values within F[_].
   *
   * This method is useful when calling .flatMap over a
   * F[StreamingT[F, A]] value.
   */
  def concat(rhs: F[StreamingT[F, A]])(implicit ev: Monad[F]): StreamingT[F, A] =
    this match {
      case This(a, ft) => This(a, ft.flatMap(_ fconcat rhs))
      case Next(ft) => Next(ft.flatMap(_ fconcat rhs))
      case Empty() => Next(rhs)
    }

  /**
   * Concatenate streaming values within F[_].
   *
   * This method is useful when calling .flatMap over a
   * F[StreamingT[F, A]] value.
   */
  def fconcat(rhs: F[StreamingT[F, A]])(implicit ev: Monad[F]): F[StreamingT[F, A]] =
    this match {
      case This(a, ft) => ev.pure(This(a, ft.flatMap(_ fconcat rhs)))
      case Next(ft) => ft.flatMap(_ fconcat rhs)
      case Empty() => rhs
    }

  /**
   * Zip two streams together.
   *
   * The lenght of the result will be the shorter of the two
   * arguments.
   */
  def zip[B](rhs: StreamingT[F, B])(implicit ev: Monad[F]): StreamingT[F, (A, B)] =
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
  def izip[B](rhs: StreamingT[F, B])(implicit ev: Monad[F]): StreamingT[F, Ior[A, B]] =
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
      case This(a, ft) => if (f(a)) ev.pure(true) else ft.flatMap(_.exists(f))
      case Next(ft) => ft.flatMap(_.exists(f))
      case Empty() => ev.pure(false)
    }

  /**
   * Return true if every element of the stream satisfies the
   * predicate, false otherwise.
   */
  def forall(f: A => Boolean)(implicit ev: Monad[F]): F[Boolean] =
    this match {
      case This(a, ft) => if (!f(a)) ev.pure(false) else ft.flatMap(_.forall(f))
      case Next(ft) => ft.flatMap(_.forall(f))
      case Empty() => ev.pure(true)
    }

  /**
   * Return a stream consisting only of the first `n` elements of this
   * stream.
   *
   * If the current stream has `n` or fewer elements, the entire
   * stream will be returned.
   */
  def take(n: Int)(implicit ev: Functor[F]): StreamingT[F, A] =
    if (n <= 0) Empty() else this match {
      case This(a, ft) => This(a, ft.map(_.take(n - 1)))
      case Next(ft) => Next(ft.map(_.take(n)))
      case Empty() => Empty()
    }

  /**
   * Return a stream consisting of all but the first `n` elements of
   * this stream.
   *
   * If the current stream has `n` or fewer elements, an empty stream
   * will be returned.
   */
  def drop(n: Int)(implicit ev: Functor[F]): StreamingT[F, A] =
    if (n <= 0) this else this match {
      case This(a, ft) => Next(ft.map(_.take(n - 1)))
      case Next(ft) => Next(ft.map(_.drop(n)))
      case Empty() => Empty()
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
  def takeWhile(f: A => Boolean)(implicit ev: Functor[F]): StreamingT[F, A] =
    this match {
      case This(a, ft) => if (f(a)) This(a, ft.map(_.takeWhile(f))) else Empty()
      case Next(ft) => Next(ft.map(_.takeWhile(f)))
      case Empty() => Empty()
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
  def dropWhile(f: A => Boolean)(implicit ev: Functor[F]): StreamingT[F, A] =
    this match {
      case This(a, ft) => if (f(a)) Empty() else This(a, ft.map(_.takeWhile(f)))
      case Next(ft) => Next(ft.map(_.dropWhile(f)))
      case Empty() => Empty()
    }

  /**
   * Provide a list of elements in the stream.
   *
   * This will evaluate the stream immediately, and will hang in the
   * case of infinite streams.
   */
  def toList(implicit ev: Monad[F]): F[List[A]] =
    this match {
      case This(a, ft) => ft.flatMap(_.toList).map(a :: _)
      case Next(ft) => ft.flatMap(_.toList)
      case Empty() => ev.pure(Nil)
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
      case This(a, _) => s"StreamingT($a, ...)"
      case Next(_) => "StreamingT(...)"
      case Empty() => "StreamingT()"
    }
}

object StreamingT extends StreamingTInstances {

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
  final case class Empty[F[_], A]() extends StreamingT[F, A]
  final case class Next[F[_], A](next: F[StreamingT[F, A]]) extends StreamingT[F, A]
  final case class This[F[_], A](a: A, tail: F[StreamingT[F, A]]) extends StreamingT[F, A]

  /**
   * Create an empty stream of type A.
   */
  def empty[F[_], A]: StreamingT[F, A] =
    Empty()

  /**
   * Alias for `.empty[F, A]`.
   */
  def apply[F[_], A]: StreamingT[F, A] =
    Empty()

  /**
   * Create a stream consisting of a single `A` value.
   */
  def apply[F[_], A](a: A)(implicit ev: Applicative[F]): StreamingT[F, A] =
    This(a, ev.pure(Empty()))

  /**
   * Create a stream from two or more values.
   */
  def apply[F[_], A](a1: A, a2: A, as: A*)(implicit ev: Applicative[F]): StreamingT[F, A] =
    This(a1, ev.pure(This(a2, ev.pure(StreamingT.fromVector[F, A](as.toVector)))))

  /**
   * Create a stream from a vector.
   */
  def fromVector[F[_], A](as: Vector[A])(implicit ev: Applicative[F]): StreamingT[F, A] = {
    def loop(s: StreamingT[F, A], i: Int): StreamingT[F, A] =
      if (i < 0) s else loop(This(as(i), ev.pure(s)), i - 1)
    loop(Empty(), as.length - 1)
  }

  /**
   * Create a stream from a list.
   */
  def fromList[F[_], A](as: List[A])(implicit ev: Applicative[F]): StreamingT[F, A] = {
    def loop(s: StreamingT[F, A], ras: List[A]): StreamingT[F, A] =
      ras match {
        case Nil => s
        case a :: rt => loop(This(a, ev.pure(s)), rt)
      }
    loop(Empty(), as.reverse)
  }

  /**
   * Create a stream consisting of a single `F[A]`.
   */
  def single[F[_], A](a: F[A])(implicit ev: Applicative[F]): StreamingT[F, A] =
    Next(a.map(apply(_)))

  /**
   * Prepend an `A` to an `F[StreamingT[F, A]]`.
   */
  def cons[F[_], A](a: A, fs: F[StreamingT[F, A]]): StreamingT[F, A] =
    This(a, fs)

  /**
   * Prepend an `A` to an `Eval[StreamingT[F, A]]`.
   */
  def lcons[F[_], A](a: A, ls: Eval[StreamingT[F, A]])(implicit ev: Applicative[F]): StreamingT[F, A] =
    This(a, ev.pureEval(ls))

  /**
   * Prepend an `F[A]` to an `F[StreamingT[F, A]]`.
   */
  def fcons[F[_]: Functor, A](fa: F[A], fs: F[StreamingT[F, A]]): StreamingT[F, A] =
    Next(fa.map(a => This(a, fs)))

  /**
   * Prepend a `StreamingT[F, A]` to an `F[StreamingT[F, A]]`.
   */
  def concat[F[_]: Monad, A](s: StreamingT[F, A], fs: F[StreamingT[F, A]]): StreamingT[F, A] =
    s concat fs

  /**
   * Prepend a `StreamingT[F, A]` to an `Eval[StreamingT[F, A]]`.
   */
  def lconcat[F[_], A](s: StreamingT[F, A], ls: Eval[StreamingT[F, A]])(implicit ev: Monad[F]): StreamingT[F, A] =
    s concat ev.pureEval(ls)

  /**
   * Prepend an `F[StreamingT[F, A]]` to an `F[StreamingT[F, A]]`.
   */
  def fconcat[F[_]: Monad, A](fs1: F[StreamingT[F, A]], fs2: F[StreamingT[F, A]]): StreamingT[F, A] =
    Next(fs1.map(_ concat fs2))

  /**
   * Produce a stream given an "unfolding" function.
   *
   * None represents an empty stream. Some(a) reprsents an initial
   * element, and we can compute the tail (if any) via f(a).
   */
  def unfold[F[_], A](o: Option[A])(f: A => F[Option[A]])(implicit ev: Functor[F]): StreamingT[F, A] =
    o match {
      case Some(a) => This(a, f(a).map(o => unfold(o)(f)))
      case None => Empty()
    }

  /**
   * Contains syntax for F[Streaming[F, A]].
   *
   * To eanble this, say:
   *
   *   import cats.data.StreamingT.syntax._
   *
   * This provides the %:: and %::: operators for prepending to an
   * F[Streaming[F, A]] value, as well as a lazy Streaming[F, A]
   * value. This mirrors the methods of the same name which can be
   * used to prepend to a Streaming[F, A] value.
   *
   * In order to support laziness when using F[Streaming[F, A]]
   * values, the type constructor F[_] must support laziness, and the
   * F[Streaming[F, A]] value must be constructed lazily.
   *
   * For example, `StreamingT[Option, ?]` cannot support laziness,
   * because Option[_] is eager.
   *
   * Additionally, `x %:: Future.successful(xs)` will not produce a
   * lazy StreamT[Future, ?], since `xs` will already have been
   * evaluated.
   */
  object syntax {
    implicit class StreamingTOps[F[_], A](rhs: => StreamingT[F, A]) {
      def %::(a: A)(implicit ev: Applicative[F]): StreamingT[F, A] =
        This(a, ev.pureEval(Always(rhs)))
      def %:::(s: StreamingT[F, A])(implicit ev: Monad[F]): StreamingT[F, A] =
        s concat ev.pureEval(Always(rhs))
      def %::(fa: F[A])(implicit ev: Monad[F]): StreamingT[F, A] =
        Next(fa.map(a => This(a, ev.pureEval(Always(rhs)))))
      def %:::(fs: F[StreamingT[F, A]])(implicit ev: Monad[F]): StreamingT[F, A] =
        Next(fs.map(_ concat ev.pureEval(Always(rhs))))
    }

    implicit class FStreamingTOps[F[_], A](rhs: F[StreamingT[F, A]]) {
      def %::(a: A): StreamingT[F, A] =
        This(a, rhs)
      def %:::(s: StreamingT[F, A])(implicit ev: Monad[F]): StreamingT[F, A] =
        s concat rhs
      def %::(fa: F[A])(implicit ev: Functor[F]): StreamingT[F, A] =
        Next(fa.map(a => This(a, rhs)))
      def %:::(fs: F[StreamingT[F, A]])(implicit ev: Monad[F]): StreamingT[F, A] =
        Next(fs.map(_ concat rhs))
    }
  }
}

trait StreamingTInstances {

  implicit def streamTMonad[F[_]: Monad]: Monad[StreamingT[F, ?]] =
    new Monad[StreamingT[F, ?]] {
      def pure[A](a: A): StreamingT[F, A] =
        StreamingT(a)
      def flatMap[A, B](fa: StreamingT[F, A])(f: A => StreamingT[F, B]): StreamingT[F, B] =
        fa.flatMap(f)
    }
}
