package cats
package data

import cats.syntax.all._

/**
 * StreamingT[F, A] is a monad transformer which parallels Streaming[A].
 *
 * However, there are a few key differences. `StreamingT[F, A]` only
 * supports lazy evaluation if `F` does, and if the `F[_]` values are
 * constructed lazily. Also, monadic recursion on `StreamingT[F, A]`
 * is stack-safe only if monadic recursion on `F` is stack-safe.
 * Finally, since `F` is not guaranteed to have a `Comonad`, it does
 * not support many methods on `Streaming[A]` which return immediate
 * values.
 */
sealed abstract class StreamingT[F[_], A] extends Product with Serializable { lhs =>

  import StreamingT.{Empty, Wait, Cons}

  /**
   * Deconstruct a stream into a head and tail (if available).
   *
   * This method will evaluate the stream until it finds a head and
   * tail, or until the stream is exhausted.
   */
  def uncons(implicit ev: Monad[F]): F[Option[(A, F[StreamingT[F, A]])]] =
    this match {
      case Cons(a, ft) => ev.pure(Some((a, ft)))
      case Wait(ft) => ft.flatMap(_.uncons)
      case Empty() => ev.pure(None)
    }

  /**
   * Lazily transform the stream given a function `f`.
   */
  def map[B](f: A => B)(implicit ev: Functor[F]): StreamingT[F, B] =
    this match {
      case Cons(a, ft) => Cons(f(a), ft.map(_.map(f)))
      case Wait(ft) => Wait(ft.map(_.map(f)))
      case Empty() => Empty()
    }

  /**
   * Lazily transform the stream given a function `f`.
   */
  def flatMap[B](f: A => StreamingT[F, B])(implicit ev: Monad[F]): StreamingT[F, B] = {
    this match {
      case Cons(a, ft) =>
        Wait(f(a) fconcat ft.map(_.flatMap(f)))
      case Wait(ft) =>
        Wait(ft.map(_.flatMap(f)))
      case Empty() =>
        Empty()
    }
  }

  /**
   * xyz
   */
  def coflatMap[B](f: StreamingT[F, A] => B)(implicit ev: Functor[F]): StreamingT[F, B] =
    this match {
      case Cons(a, ft) => Cons(f(this), ft.map(_.coflatMap(f)))
      case Wait(ft) => Wait(ft.map(_.coflatMap(f)))
      case Empty() => Empty()
    }

  /**
   * Lazily filter the stream given the predicate `f`.
   */
  def filter(f: A => Boolean)(implicit ev: Functor[F]): StreamingT[F, A] =
    this match {
      case Cons(a, ft) =>
        val tail = ft.map(_.filter(f))
        if (f(a)) Cons(a, tail) else Wait(tail)
      case Wait(ft) => Wait(ft.map(_.filter(f)))
      case Empty() => this
    }

  /**
   * Eagerly fold the stream to a single value from the left.
   */
  def foldLeft[B](b: B)(f: (B, A) => B)(implicit ev: Monad[F]): F[B] =
    this match {
      case Cons(a, ft) => ft.flatMap(_.foldLeft(f(b, a))(f))
      case Wait(ft) => ft.flatMap(_.foldLeft(b)(f))
      case Empty() => ev.pure(b)
    }

  /**
   * Eagerly search the stream from the left. The search ends when f
   * returns true for some a, or the stream is exhausted. Some(a)
   * signals a match, None means no matching items were found.
   */
  def find(f: A => Boolean)(implicit ev: Monad[F]): F[Option[A]] =
    this match {
      case Cons(a, ft) =>
        if (f(a)) ev.pure(Some(a)) else ft.flatMap(_.find(f))
      case Wait(ft) =>
        ft.flatMap(_.find(f))
      case Empty() =>
        ev.pure(None)
    }

  /**
   * Return true if the stream is empty, false otherwise.
   *
   * In this case of deferred streams this will force the first
   * element to be calculated.
   */
  def isEmpty(implicit ev: Monad[F]): F[Boolean] =
    uncons.map(_.isEmpty)

  /**
   * Return true if the stream is non-empty, false otherwise.
   *
   * In this case of deferred streams this will force the first
   * element to be calculated.
   */
  def nonEmpty(implicit ev: Monad[F]): F[Boolean] =
    uncons.map(_.isDefined)

  /**
   * Prepend an A value to the current stream.
   */
  def %::(a: A)(implicit ev: Applicative[F]): StreamingT[F, A] =
    Cons(a, ev.pure(this))

  /**
   * Prepend a StreamingT[F, A] value to the current stream.
   */
  def %:::(lhs: StreamingT[F, A])(implicit ev: Functor[F]): StreamingT[F, A] =
    lhs match {
      case Cons(a, ft) => Cons(a, ft.map(_ %::: this))
      case Wait(ft) => Wait(ft.map(_ %::: this))
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
      case Cons(a, ft) => Cons(a, ft.flatMap(_ fconcat rhs))
      case Wait(ft) => Wait(ft.flatMap(_ fconcat rhs))
      case Empty() => Wait(rhs)
    }

  /**
   * Concatenate streaming values within F[_].
   *
   * This method is useful when calling .flatMap over a
   * F[StreamingT[F, A]] value.
   */
  def fconcat(rhs: F[StreamingT[F, A]])(implicit ev: Monad[F]): F[StreamingT[F, A]] =
    this match {
      case Cons(a, ft) => ev.pure(Cons(a, ft.flatMap(_ fconcat rhs)))
      case Wait(ft) => ft.flatMap(_ fconcat rhs)
      case Empty() => rhs
    }

  /**
   * Return true if some element of the stream satisfies the
   * predicate, false otherwise.
   */
  def exists(f: A => Boolean)(implicit ev: Monad[F]): F[Boolean] =
    this match {
      case Cons(a, ft) => if (f(a)) ev.pure(true) else ft.flatMap(_.exists(f))
      case Wait(ft) => ft.flatMap(_.exists(f))
      case Empty() => ev.pure(false)
    }

  /**
   * Return true if every element of the stream satisfies the
   * predicate, false otherwise.
   */
  def forall(f: A => Boolean)(implicit ev: Monad[F]): F[Boolean] =
    this match {
      case Cons(a, ft) => if (!f(a)) ev.pure(false) else ft.flatMap(_.forall(f))
      case Wait(ft) => ft.flatMap(_.forall(f))
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
      case Cons(a, ft) => Cons(a, ft.map(_.take(n - 1)))
      case Wait(ft) => Wait(ft.map(_.take(n)))
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
      case Cons(a, ft) => Wait(ft.map(_.drop(n - 1)))
      case Wait(ft) => Wait(ft.map(_.drop(n)))
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
   * {{{
   * scala> import cats.std.list._
   * scala> val s = StreamingT[List, Int](1, 2, 3, 4, 5, 6, 7)
   * scala> s.takeWhile(n => n != 4).toList.flatten
   * res0: List[Int] = List(1, 2, 3)
   * }}}
   */
  def takeWhile(f: A => Boolean)(implicit ev: Functor[F]): StreamingT[F, A] =
    this match {
      case Cons(a, ft) => if (f(a)) Cons(a, ft.map(_.takeWhile(f))) else Empty()
      case Wait(ft) => Wait(ft.map(_.takeWhile(f)))
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
   * {{{
   * scala> import cats.std.list._
   * scala> val s = StreamingT[List, Int](1, 2, 3, 4, 5, 6, 7)
   * scala> s.dropWhile(n => n != 4).toList.flatten
   * res0: List[Int] = List(4, 5, 6, 7)
   * }}}
   */
  def dropWhile(f: A => Boolean)(implicit ev: Functor[F]): StreamingT[F, A] =
    this match {
      case s @ Cons(a, ft) => if (f(a)) Wait(ft.map(_.dropWhile(f))) else s
      case Wait(ft) => Wait(ft.map(_.dropWhile(f)))
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
      case Cons(a, ft) => ft.flatMap(_.toList).map(a :: _)
      case Wait(ft) => ft.flatMap(_.toList)
      case Empty() => ev.pure(Nil)
    }

  /**
   * Basic string representation of a stream.
   *
   * This method will not force evaluation of any lazy part of a
   * stream. As a result, you will see at most one element (the first
   * one).
   */
  override def toString: String = this match {
    case Cons(a, _) => s"StreamingT($a, ...)"
    case Wait(_) => "StreamingT(...)"
    case Empty() => "StreamingT()"
  }
}

object StreamingT extends StreamingTInstances {

  /**
   * Concrete StreamingT[A] types:
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
  private[cats] final case class Empty[F[_], A]() extends StreamingT[F, A]
  private[cats] final case class Wait[F[_], A](next: F[StreamingT[F, A]]) extends StreamingT[F, A]
  private[cats] final case class Cons[F[_], A](a: A, tail: F[StreamingT[F, A]]) extends StreamingT[F, A]

  /**
   * Create an empty stream of type A.
   */
  def empty[F[_], A]: StreamingT[F, A] =
    Empty()

  /**
   * Create a stream consisting of a single `A` value.
   */
  def apply[F[_], A](a: A)(implicit ev: Applicative[F]): StreamingT[F, A] =
    Cons(a, ev.pure(Empty()))

  /**
   * Create a stream from two or more values.
   */
  def apply[F[_], A](a1: A, a2: A, as: A*)(implicit ev: Applicative[F]): StreamingT[F, A] =
    Cons(a1, ev.pure(Cons(a2, ev.pure(StreamingT.fromVector[F, A](as.toVector)))))

  /**
   * Create a stream from a vector.
   */
  def fromVector[F[_], A](as: Vector[A])(implicit ev: Applicative[F]): StreamingT[F, A] = {
    def loop(s: StreamingT[F, A], i: Int): StreamingT[F, A] =
      if (i < 0) s else loop(Cons(as(i), ev.pure(s)), i - 1)
    loop(Empty(), as.length - 1)
  }

  /**
   * Create a stream from a list.
   */
  def fromList[F[_], A](as: List[A])(implicit ev: Applicative[F]): StreamingT[F, A] = {
    def loop(s: StreamingT[F, A], ras: List[A]): StreamingT[F, A] =
      ras match {
        case Nil => s
        case a :: rt => loop(Cons(a, ev.pure(s)), rt)
      }
    loop(Empty(), as.reverse)
  }

  /**
   * Create a stream consisting of a single `F[A]`.
   */
  def single[F[_]: Applicative, A](a: F[A]): StreamingT[F, A] =
    Wait(a.map(apply(_)))

  /**
   * Create a stream from `A` and `F[StreamingT[F, A]]` values.
   */
  def cons[F[_], A](a: A, fs: F[StreamingT[F, A]]): StreamingT[F, A] =
    Cons(a, fs)

  /**
   * Create a stream from a deferred `StreamingT[F, A]` value.
   * Note: the extent to which this defers the value depends on the `pureEval`
   * implementation of the `Applicative[F]` instance.
   */
  def defer[F[_], A](s: => StreamingT[F, A])(implicit ev: Applicative[F]): StreamingT[F, A] =
    Wait(ev.pureEval(Always(s)))

  /**
   * Create a stream from an `F[StreamingT[F, A]]` value.
   */
  def wait[F[_], A](fs: F[StreamingT[F, A]]): StreamingT[F, A] =
    Wait(fs)

  /**
   * Produce a stream given an "unfolding" function.
   *
   * None represents an empty stream. Some(a) reprsents an initial
   * element, and we can compute the tail (if any) via f(a).
   */
  def unfold[F[_]: Functor, A](o: Option[A])(f: A => F[Option[A]]): StreamingT[F, A] =
    o match {
      case Some(a) =>
        Cons(a, f(a).map(o => unfold(o)(f)))
      case None =>
        Empty()
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
    implicit final class StreamingTOps[F[_], A](rhs: => StreamingT[F, A]) {
      def %::(a: A)(implicit ev: Applicative[F]): StreamingT[F, A] =
        Cons(a, ev.pureEval(Always(rhs)))
      def %:::(s: StreamingT[F, A])(implicit ev: Monad[F]): StreamingT[F, A] =
        s concat ev.pureEval(Always(rhs))
      def %::(fa: F[A])(implicit ev: Monad[F]): StreamingT[F, A] =
        Wait(fa.map(a => Cons(a, ev.pureEval(Always(rhs)))))
      def %:::(fs: F[StreamingT[F, A]])(implicit ev: Monad[F]): StreamingT[F, A] =
        Wait(fs.map(_ concat ev.pureEval(Always(rhs))))
    }

    implicit final class FStreamingTOps[F[_], A](rhs: F[StreamingT[F, A]]) {
      def %::(a: A): StreamingT[F, A] =
        Cons(a, rhs)
      def %:::(s: StreamingT[F, A])(implicit ev: Monad[F]): StreamingT[F, A] =
        s concat rhs
      def %::(fa: F[A])(implicit ev: Functor[F]): StreamingT[F, A] =
        Wait(fa.map(a => Cons(a, rhs)))
      def %:::(fs: F[StreamingT[F, A]])(implicit ev: Monad[F]): StreamingT[F, A] =
        Wait(fs.map(_ concat rhs))
    }
  }
}

private[data] sealed trait StreamingTInstances extends StreamingTInstances1 {

  implicit def streamingTInstance[F[_]: Monad]: MonadCombine[StreamingT[F, ?]] with CoflatMap[StreamingT[F, ?]] =
    new MonadCombine[StreamingT[F, ?]] with CoflatMap[StreamingT[F, ?]] {
      def pure[A](a: A): StreamingT[F, A] =
        StreamingT(a)
      def flatMap[A, B](fa: StreamingT[F, A])(f: A => StreamingT[F, B]): StreamingT[F, B] =
        fa.flatMap(f)
      def empty[A]: StreamingT[F, A] =
        StreamingT.empty
      def combineK[A](xs: StreamingT[F, A], ys: StreamingT[F, A]): StreamingT[F, A] =
        xs %::: ys
      override def filter[A](fa: StreamingT[F, A])(f: A => Boolean): StreamingT[F, A] =
        fa.filter(f)
      def coflatMap[A, B](fa: StreamingT[F, A])(f: StreamingT[F, A] => B): StreamingT[F, B] =
        fa.coflatMap(f)

      override def map[A, B](fa: StreamingT[F, A])(f: A => B): StreamingT[F, B] =
        fa.map(f)
    }

  implicit def streamingTOrder[F[_], A](implicit ev: Monad[F], eva: Order[F[List[A]]]): Order[StreamingT[F, A]] =
    new Order[StreamingT[F, A]] {
      def compare(x: StreamingT[F, A], y: StreamingT[F, A]): Int =
        x.toList compare y.toList
    }

  implicit def streamingTTransLift[M[_]: Applicative]: TransLift[StreamingT, M] =
    new TransLift[StreamingT, M] {
      def liftT[A](ma: M[A]): StreamingT[M, A] = StreamingT.single(ma)
    }
}

private[data] sealed trait StreamingTInstances1 extends StreamingTInstances2 {
  implicit def streamingTPartialOrder[F[_], A](implicit ev: Monad[F], eva: PartialOrder[F[List[A]]]): PartialOrder[StreamingT[F, A]] =
    new PartialOrder[StreamingT[F, A]] {
      def partialCompare(x: StreamingT[F, A], y: StreamingT[F, A]): Double =
        x.toList partialCompare y.toList
    }
}

private[data] sealed trait StreamingTInstances2 {
  implicit def streamingTEq[F[_], A](implicit ev: Monad[F], eva: Eq[F[List[A]]]): Eq[StreamingT[F, A]] =
    new Eq[StreamingT[F, A]] {
      def eqv(x: StreamingT[F, A], y: StreamingT[F, A]): Boolean =
        x.toList === y.toList
    }
}
