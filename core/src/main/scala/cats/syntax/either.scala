/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats
package syntax

import cats.data._

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}
import EitherSyntax._

trait EitherSyntax {
  implicit final def catsSyntaxEither[A, B](eab: Either[A, B]): EitherOps[A, B] = new EitherOps(eab)

  implicit final def catsSyntaxEitherObject(either: Either.type): EitherObjectOps =
    new EitherObjectOps(either)

  implicit final def catsSyntaxLeft[A, B](left: Left[A, B]): LeftOps[A, B] = new LeftOps(left)

  implicit final def catsSyntaxRight[A, B](right: Right[A, B]): RightOps[A, B] = new RightOps(right)

  implicit final def catsSyntaxEitherId[A](a: A): EitherIdOps[A] = new EitherIdOps(a)
}

object EitherSyntax {

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[syntax] class CatchOnlyPartiallyApplied[T](private val dummy: Boolean = true) extends AnyVal {
    def apply[A](f: => A)(implicit CT: ClassTag[T], NT: NotNull[T]): Either[T, A] =
      try {
        Right(f)
      } catch {
        case t if CT.runtimeClass.isInstance(t) =>
          Left(t.asInstanceOf[T])
      }
  }
}

final class EitherOps[A, B](private val eab: Either[A, B]) extends AnyVal {
  @deprecated("Included in the standard library", "2.1.0-RC1")
  private[syntax] def foreach(f: B => Unit): Unit =
    eab match {
      case Left(_)  => ()
      case Right(b) => f(b)
    }

  @deprecated("Included in the standard library", "2.1.0-RC1")
  private[syntax] def getOrElse[BB >: B](default: => BB): BB =
    eab match {
      case Left(_)  => default
      case Right(b) => b
    }

  def orElse[C, BB >: B](fallback: => Either[C, BB]): Either[C, BB] =
    eab match {
      case Left(_)      => fallback
      case r @ Right(_) => EitherUtil.leftCast(r)
    }

  def recover[BB >: B](pf: PartialFunction[A, BB]): Either[A, BB] =
    eab match {
      case Left(a) if pf.isDefinedAt(a) => Right(pf(a))
      case _                            => eab
    }

  def recoverWith[AA >: A, BB >: B](pf: PartialFunction[A, Either[AA, BB]]): Either[AA, BB] =
    eab match {
      case Left(a) if pf.isDefinedAt(a) => pf(a)
      case _                            => eab
    }

  def valueOr[BB >: B](f: A => BB): BB =
    eab match {
      case Left(a)  => f(a)
      case Right(b) => b
    }

  @deprecated("Included in the standard library", "2.1.0-RC1")
  private[syntax] def forall(f: B => Boolean): Boolean =
    eab match {
      case Left(_)  => true
      case Right(b) => f(b)
    }

  @deprecated("Included in the standard library", "2.1.0-RC1")
  private[syntax] def exists(f: B => Boolean): Boolean =
    eab match {
      case Left(_)  => false
      case Right(b) => f(b)
    }

  def ensure[AA >: A](onFailure: => AA)(f: B => Boolean): Either[AA, B] =
    eab match {
      case Left(_)  => eab
      case Right(b) => if (f(b)) eab else Left(onFailure)
    }

  def ensureOr[AA >: A](onFailure: B => AA)(f: B => Boolean): Either[AA, B] =
    eab match {
      case Left(_)  => eab
      case Right(b) => if (f(b)) eab else Left(onFailure(b))
    }

  def toIor: A Ior B = Ior.fromEither(eab)

  @deprecated("Included in the standard library", "2.1.0-RC1")
  private[syntax] def toOption: Option[B] =
    eab match {
      case Left(_)  => None
      case Right(b) => Some(b)
    }

  def toList: List[B] =
    eab match {
      case Left(_)  => Nil
      case Right(b) => List(b)
    }

  @deprecated("Included in the standard library", "2.1.0-RC1")
  private[syntax] def toTry(implicit ev: A <:< Throwable): Try[B] =
    eab match {
      case Left(a)  => Failure(ev(a))
      case Right(b) => Success(b)
    }

  def toValidated: Validated[A, B] =
    eab match {
      case Left(a)  => Validated.invalid(a)
      case Right(b) => Validated.valid(b)
    }

  /**
   * Returns a [[cats.data.ValidatedNel]] representation of this disjunction with the `Left` value
   * as a single element on the `Invalid` side of the [[cats.data.NonEmptyList]].
   */
  def toValidatedNel[AA >: A]: ValidatedNel[AA, B] =
    eab match {
      case Left(a)  => Validated.invalidNel(a)
      case Right(b) => Validated.valid(b)
    }

  def withValidated[AA, BB](f: Validated[A, B] => Validated[AA, BB]): Either[AA, BB] =
    f(toValidated).toEither

  def to[F[_]](implicit F: Alternative[F]): F[B] =
    eab match {
      case Left(_)  => F.empty
      case Right(b) => F.pure(b)
    }

  def bimap[C, D](fa: A => C, fb: B => D): Either[C, D] =
    eab match {
      case Left(a)  => Left(fa(a))
      case Right(b) => Right(fb(b))
    }

  @deprecated("Included in the standard library", "2.1.0-RC1")
  private[syntax] def map[C](f: B => C): Either[A, C] =
    eab match {
      case l @ Left(_) => EitherUtil.rightCast(l)
      case Right(b)    => Right(f(b))
    }

  def map2Eval[AA >: A, C, Z](fc: Eval[Either[AA, C]])(f: (B, C) => Z): Eval[Either[AA, Z]] =
    eab match {
      case l @ Left(_) => Now(EitherUtil.rightCast(l))
      case Right(b)    => fc.map(_.map(f(b, _)))
    }

  def leftMap[C](f: A => C): Either[C, B] =
    eab match {
      case Left(a)      => Left(f(a))
      case r @ Right(_) => EitherUtil.leftCast(r)
    }

  @deprecated("Included in the standard library", "2.1.0-RC1")
  private[syntax] def flatMap[AA >: A, D](f: B => Either[AA, D]): Either[AA, D] =
    eab match {
      case l @ Left(_) => EitherUtil.rightCast(l)
      case Right(b)    => f(b)
    }

  def leftFlatMap[C, BB >: B](f: A => Either[C, BB]): Either[C, BB] =
    eab match {
      case Left(a)      => f(a)
      case r @ Right(_) => EitherUtil.leftCast(r)
    }

  def compare[AA >: A, BB >: B](that: Either[AA, BB])(implicit AA: Order[AA], BB: Order[BB]): Int =
    eab match {
      case Left(a1) =>
        that match {
          case Left(a2) => AA.compare(a1, a2)
          case Right(_) => -1
        }
      case Right(b1) =>
        that match {
          case Left(_)   => 1
          case Right(b2) => BB.compare(b1, b2)
        }
    }

  def partialCompare[AA >: A, BB >: B](
    that: Either[AA, BB]
  )(implicit AA: PartialOrder[AA], BB: PartialOrder[BB]): Double =
    eab match {
      case Left(a1) =>
        that match {
          case Left(a2) => AA.partialCompare(a1, a2)
          case Right(_) => -1
        }
      case Right(b1) =>
        that match {
          case Left(_)   => 1
          case Right(b2) => BB.partialCompare(b1, b2)
        }
    }

  def ===[AA >: A, BB >: B](that: Either[AA, BB])(implicit AA: Eq[AA], BB: Eq[BB]): Boolean =
    eab match {
      case Left(a1) =>
        that match {
          case Left(a2) => AA.eqv(a1, a2)
          case Right(_) => false
        }
      case Right(b1) =>
        that match {
          case Left(_)   => false
          case Right(b2) => BB.eqv(b1, b2)
        }
    }

  def traverse[F[_], AA >: A, D](f: B => F[D])(implicit F: Applicative[F]): F[Either[AA, D]] =
    eab match {
      case l @ Left(_) => F.pure(EitherUtil.rightCast(l))
      case Right(b)    => F.map(f(b))(Right(_))
    }

  def foldLeft[C](c: C)(f: (C, B) => C): C =
    eab match {
      case Left(_)  => c
      case Right(b) => f(c, b)
    }

  def foldRight[C](lc: Eval[C])(f: (B, Eval[C]) => Eval[C]): Eval[C] =
    eab match {
      case Left(_)  => lc
      case Right(b) => f(b, lc)
    }

  /**
   * Combine with another `Either` value.
   *
   * If this `Either` is a `Left` then it will be returned as-is.
   * If this `Either` is a `Right` and `that` `Either` is a left, then `that` will be
   * returned.
   * If both `Either`s are `Right`s, then the `Semigroup[BB]` instance will be used
   * to combine both values and return them as a `Right`.
   * Note: If both `Either`s are `Left`s then their values are not combined. Use
   * `Validated` if you prefer to combine `Left` values.
   *
   * Examples:
   * {{{
   * scala> import cats.syntax.all._
   * scala> val l1: Either[String, Int] = Either.left("error 1")
   * scala> val l2: Either[String, Int] = Either.left("error 2")
   * scala> val r3: Either[String, Int] = Either.right(3)
   * scala> val r4: Either[String, Int] = Either.right(4)
   *
   * scala> l1 combine l2
   * res0: Either[String, Int] = Left(error 1)
   *
   * scala> l1 combine r3
   * res1: Either[String, Int] = Left(error 1)
   *
   * scala> r3 combine l1
   * res2: Either[String, Int] = Left(error 1)
   *
   * scala> r3 combine r4
   * res3: Either[String, Int] = Right(7)
   * }}}
   */
  final def combine[AA >: A, BB >: B](that: Either[AA, BB])(implicit BB: Semigroup[BB]): Either[AA, BB] =
    eab match {
      case left @ Left(_) => left
      case Right(b1) =>
        that match {
          case left @ Left(_) => left
          case Right(b2)      => Right(BB.combine(b1, b2))
        }
    }

  def show[AA >: A, BB >: B](implicit AA: Show[AA], BB: Show[BB]): String =
    eab match {
      case Left(a)  => s"Left(${AA.show(a)})"
      case Right(b) => s"Right(${BB.show(b)})"
    }

  def ap[AA >: A, BB >: B, C](that: Either[AA, BB => C]): Either[AA, C] = that.flatMap(eab.map)

  /**
   * Transform the `Either` into a [[cats.data.EitherT]] while lifting it into the specified Applicative.
   *
   * {{{
   * scala> import cats.syntax.all._
   * scala> val e: Either[String, Int] = Right(3)
   * scala> e.toEitherT[Option]
   * res0: cats.data.EitherT[Option, String, Int] = EitherT(Some(Right(3)))
   * }}}
   */
  def toEitherT[F[_]: Applicative]: EitherT[F, A, B] = EitherT.fromEither(eab)

  def toEitherNec[AA >: A]: EitherNec[AA, B] = leftMap(NonEmptyChain.one)

  def toEitherNes[AA >: A](implicit O: Order[AA]): EitherNes[AA, B] = leftMap(NonEmptySet.one(_))

  def toEitherNel[AA >: A]: EitherNel[AA, B] = leftMap(NonEmptyList.one)

  @deprecated("use liftTo instead", "2.0.0")
  def raiseOrPure[F[_]](implicit ev: ApplicativeError[F, A]): F[B] =
    ev.fromEither(eab)

  /**
   * lift the `Either` into a `F[_]` with `ApplicativeError[F, A]` instance
   *
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   * scala> val e: Either[String, Int] = Right(3)
   * scala> e.liftTo[EitherT[Option, CharSequence, *]]
   * res0: cats.data.EitherT[Option, CharSequence, Int] = EitherT(Some(Right(3)))
   * }}}
   */
  def liftTo[F[_]](implicit F: ApplicativeError[F, ? >: A]): F[B] = F.fromEither(eab)
}

final class EitherObjectOps(private val either: Either.type) extends AnyVal {
  def left[A, B](a: A): Either[A, B] = Left(a)

  def right[A, B](b: B): Either[A, B] = Right(b)

  def leftNec[A, B](a: A): EitherNec[A, B] = Left(NonEmptyChain.one(a))

  def rightNec[A, B](b: B): EitherNec[A, B] = Right(b)

  def leftNes[A, B](a: A)(implicit O: Order[A]): EitherNes[A, B] = Left(NonEmptySet.one(a))

  def rightNes[A, B](b: B)(implicit O: Order[B]): EitherNes[A, B] = Right(b)

  def leftNel[A, B](a: A): EitherNel[A, B] = Left(NonEmptyList.one(a))

  def rightNel[A, B](b: B): EitherNel[A, B] = Right(b)

  /**
   * Evaluates the specified block, catching exceptions of the specified type and returning them on the left side of
   * the resulting `Either`. Uncaught exceptions are propagated.
   *
   * For example:
   * {{{
   * scala> import cats.syntax.all._ // get syntax for Either
   * scala> Either.catchOnly[NumberFormatException] { "foo".toInt }
   * res0: Either[NumberFormatException, Int] = Left(java.lang.NumberFormatException: For input string: "foo")
   * }}}
   */
  def catchOnly[T >: Null <: Throwable]: CatchOnlyPartiallyApplied[T] =
    new CatchOnlyPartiallyApplied[T]

  def catchNonFatal[A](f: => A): Either[Throwable, A] =
    try {
      right(f)
    } catch {
      case t if scala.util.control.NonFatal(t) => left(t)
    }

  /**
   * Converts a `Try[A]` to a `Either[Throwable, A]`.
   */
  def fromTry[A](t: Try[A]): Either[Throwable, A] =
    t match {
      case Failure(e) => left(e)
      case Success(v) => right(v)
    }

  /**
   * Converts an `Option[B]` to an `Either[A, B]`, where the provided `ifNone` values is returned on
   * the left of the `Either` when the specified `Option` is `None`.
   */
  def fromOption[A, B](o: Option[B], ifNone: => A): Either[A, B] =
    o match {
      case None    => left[A, B](ifNone)
      case Some(a) => right(a)
    }

  /**
   * Cached value of `Right(())` to avoid allocations for a common case.
   */
  def unit[A]: Either[A, Unit] = EitherUtil.unit

  /**
   * Returns `Left(ifTrue)` when the `cond` is true, otherwise `Right(())`
   *
   * @example {{{
   * val tooMany = 5
   * val x: Int = ???
   * Either.raiseWhen(x >= tooMany)(new IllegalArgumentException("Too many"))
   * }}}
   */
  def raiseWhen[A](cond: Boolean)(ifTrue: => A): Either[A, Unit] =
    ApplicativeError[Either[A, *], A].raiseWhen(cond)(ifTrue)

  /**
   * Returns `Left(ifFalse)` when `cond` is false, otherwise `Right(())`
   *
   * @example {{{
   * val tooMany = 5
   * val x: Int = ???
   * Either.raiseUnless(x < tooMany)(new IllegalArgumentException("Too many"))
   * }}}
   */
  def raiseUnless[A](cond: Boolean)(ifFalse: => A): Either[A, Unit] =
    ApplicativeError[Either[A, *], A].raiseUnless(cond)(ifFalse)
}

final class LeftOps[A, B](private val left: Left[A, B]) extends AnyVal {

  /**
   * Cast the right type parameter of the `Left`.
   */
  def rightCast[C]: Either[A, C] = left.asInstanceOf[Either[A, C]]
}

final class RightOps[A, B](private val right: Right[A, B]) extends AnyVal {

  /**
   * Cast the left type parameter of the `Right`.
   */
  def leftCast[C]: Either[C, B] = right.asInstanceOf[Either[C, B]]
}

final class EitherIdOps[A](private val obj: A) extends AnyVal {

  /**
   * Wrap a value in `Left`.
   */
  def asLeft[B]: Either[A, B] = Left(obj)

  /**
   * Wrap a value in `Right`.
   */
  def asRight[B]: Either[B, A] = Right(obj)

  /**
   * Wrap a value to a left EitherNel
   *
   * For example:
   * {{{
   * scala> import cats.syntax.all._, cats.data.NonEmptyList
   * scala> "Err".leftNel[Int]
   * res0: Either[NonEmptyList[String], Int] = Left(NonEmptyList(Err))
   * }}}
   */
  def leftNel[B]: Either[NonEmptyList[A], B] = Left(NonEmptyList.one(obj))

  /**
   * Wrap a value to a right EitherNel
   *
   * For example:
   * {{{
   * scala> import cats.syntax.all._, cats.data.NonEmptyList
   * scala> 1.rightNel[String]
   * res0: Either[NonEmptyList[String], Int] = Right(1)
   * }}}
   */
  def rightNel[B]: Either[NonEmptyList[B], A] = Right(obj)

}

private[syntax] trait EitherSyntaxBinCompat0 {
  implicit final def catsSyntaxEitherBinCompat0[A, B](eab: Either[A, B]): EitherOpsBinCompat0[A, B] =
    new EitherOpsBinCompat0(eab)

  implicit final def catsSyntaxEitherIdBinCompat0[A](a: A): EitherIdOpsBinCompat0[A] =
    new EitherIdOpsBinCompat0(a)
}

final private[syntax] class EitherIdOpsBinCompat0[A](private val value: A) extends AnyVal {

  /**
   * Wrap a value to a left EitherNec
   *
   * For example:
   * {{{
   * scala> import cats.syntax.all._, cats.data.NonEmptyChain
   * scala> "Err".leftNec[Int]
   * res0: Either[NonEmptyChain[String], Int] = Left(Chain(Err))
   * }}}
   */
  def leftNec[B]: Either[NonEmptyChain[A], B] = Left(NonEmptyChain.one(value))

  /**
   * Wrap a value to a right EitherNec
   *
   * For example:
   * {{{
   * scala> import cats.syntax.all._, cats.data.NonEmptyChain
   * scala> 1.rightNec[String]
   * res0: Either[NonEmptyChain[String], Int] = Right(1)
   * }}}
   */
  def rightNec[B]: Either[NonEmptyChain[B], A] = Right(value)
}

final private[syntax] class EitherOpsBinCompat0[A, B](private val value: Either[A, B]) extends AnyVal {

  /**
   * Returns a [[cats.data.ValidatedNec]] representation of this disjunction with the `Left` value
   * as a single element on the `Invalid` side of the [[cats.data.NonEmptyList]].
   */
  def toValidatedNec: ValidatedNec[A, B] =
    value match {
      case Left(a)  => Validated.invalidNec(a)
      case Right(b) => Validated.valid(b)
    }
}

/**
 * Convenience methods to use `Either` syntax inside `Either` syntax definitions.
 */
private[cats] object EitherUtil {
  def leftCast[A, B, C](right: Right[A, B]): Either[C, B] =
    right.asInstanceOf[Either[C, B]]
  def rightCast[A, B, C](left: Left[A, B]): Either[A, C] =
    left.asInstanceOf[Either[A, C]]

  private[cats] val unit = Right(())
}
