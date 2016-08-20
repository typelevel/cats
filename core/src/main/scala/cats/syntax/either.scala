package cats
package syntax

import cats.data.{EitherT, Ior, Validated, ValidatedNel, Xor}
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

trait EitherSyntax {
  implicit def catsSyntaxEither[A, B](eab: Either[A, B]): EitherOps[A, B] = new EitherOps(eab)

  implicit def catsSyntaxEitherObject(either: Either.type): EitherObjectOps = new EitherObjectOps(either) // scalastyle:off ensure.single.space.after.token

  implicit def catsSyntaxLeft[A, B](left: Left[A, B]): LeftOps[A, B] = new LeftOps(left)

  implicit def catsSyntaxRight[A, B](right: Right[A, B]): RightOps[A, B] = new RightOps(right)
}

final class EitherOps[A, B](val eab: Either[A, B]) extends AnyVal {
  def foreach(f: B => Unit): Unit = eab match {
    case Left(_)  => ()
    case Right(b) => f(b)
  }

  def getOrElse(default: => B): B = eab match {
    case Left(_)  => default
    case Right(b) => b
  }

  def orElse[C](fallback: => Either[C, B]): Either[C, B] = eab match {
    case Left(_)      => fallback
    case r @ Right(_) => EitherUtil.leftCast(r)
  }

  def recover(pf: PartialFunction[A, B]): Either[A, B] = eab match {
    case Left(a) if pf.isDefinedAt(a) => Right(pf(a))
    case _                            => eab
  }

  def recoverWith(pf: PartialFunction[A, Either[A, B]]): Either[A, B] = eab match {
    case Left(a) if pf.isDefinedAt(a) => pf(a)
    case _                            => eab
  }

  def valueOr(f: A => B): B = eab match {
    case Left(a)  => f(a)
    case Right(b) => b
  }

  def forall(f: B => Boolean): Boolean = eab match {
    case Left(_)  => true
    case Right(b) => f(b)
  }

  def exists(f: B => Boolean): Boolean = eab match {
    case Left(_)  => false
    case Right(b) => f(b)
  }

  def ensure(onFailure: => A)(f: B => Boolean): Either[A, B] = eab match {
    case Left(_)  => eab
    case Right(b) => if (f(b)) eab else Left(onFailure)
  }

  def toIor: A Ior B = eab match {
    case Left(a)  => Ior.left(a)
    case Right(b) => Ior.right(b)
  }

  def toOption: Option[B] = eab match {
    case Left(_)  => None
    case Right(b) => Some(b)
  }

  def toList: List[B] = eab match {
    case Left(_)  => Nil
    case Right(b) => List(b)
  }

  def toTry(implicit ev: A <:< Throwable): Try[B] = eab match {
    case Left(a)  => Failure(ev(a))
    case Right(b) => Success(b)
  }

  def toValidated: Validated[A, B] = eab match {
    case Left(a)  => Validated.invalid(a)
    case Right(b) => Validated.valid(b)
  }

  /** Returns a [[cats.data.ValidatedNel]] representation of this disjunction with the `Left` value
   * as a single element on the `Invalid` side of the [[cats.data.NonEmptyList]]. */
  def toValidatedNel: ValidatedNel[A, B] = eab match {
    case Left(a)  => Validated.invalidNel(a)
    case Right(b) => Validated.valid(b)
  }

  def withValidated[AA, BB](f: Validated[A, B] => Validated[AA, BB]): Either[AA, BB] =
    f(toValidated).toEither

  def to[F[_]](implicit F: Alternative[F]): F[B] = eab match {
    case Left(_)  => F.empty
    case Right(b) => F.pure(b)
  }

  def bimap[C, D](fa: A => C, fb: B => D): Either[C, D] = eab match {
    case Left(a)  => Left(fa(a))
    case Right(b) => Right(fb(b))
  }

  def map[C](f: B => C): Either[A, C] = eab match {
    case l @ Left(_) => EitherUtil.rightCast(l)
    case Right(b)    => Right(f(b))
  }

  def map2Eval[C, Z](fc: Eval[Either[A, C]])(f: (B, C) => Z): Eval[Either[A, Z]] =
    eab match {
      case l @ Left(_) => Now(EitherUtil.rightCast(l))
      case Right(b)    => fc.map(either => new EitherOps(either).map(f(b, _)))
    }

  def leftMap[C](f: A => C): Either[C, B] = eab match {
    case Left(a)      => Left(f(a))
    case r @ Right(_) => EitherUtil.leftCast(r)
  }

  def flatMap[D](f: B => Either[A, D]): Either[A, D] = eab match {
    case l @ Left(_) => EitherUtil.rightCast(l)
    case Right(b)    => f(b)
  }

  def compare(that: Either[A, B])(implicit A: Order[A], B: Order[B]): Int = eab match {
    case Left(a1)  =>
      that match {
        case Left(a2) => A.compare(a1, a2)
        case Right(_) => -1
      }
    case Right(b1) =>
      that match {
        case Left(_)   => 1
        case Right(b2) => B.compare(b1, b2)
      }
  }

  def partialCompare(that: Either[A, B])(implicit A: PartialOrder[A], B: PartialOrder[B]): Double = eab match {
    case Left(a1)  =>
      that match {
        case Left(a2) => A.partialCompare(a1, a2)
        case Right(_) => -1
      }
    case Right(b1) =>
      that match {
        case Left(_)   => 1
        case Right(b2) => B.partialCompare(b1, b2)
      }
  }

  def ===(that: Either[A, B])(implicit A: Eq[A], B: Eq[B]): Boolean = eab match {
    case Left(a1)  =>
      that match {
        case Left(a2) => A.eqv(a1, a2)
        case Right(_) => false
      }
    case Right(b1) =>
      that match {
        case Left(_)   => false
        case Right(b2) => B.eqv(b1, b2)
      }
  }

  def traverse[F[_], D](f: B => F[D])(implicit F: Applicative[F]): F[Either[A, D]] = eab match {
    case l @ Left(_) => F.pure(EitherUtil.rightCast(l))
    case Right(b)    => F.map(f(b))(Right(_))
  }

  def foldLeft[C](c: C)(f: (C, B) => C): C = eab match {
    case Left(_)  => c
    case Right(b) => f(c, b)
  }

  def foldRight[C](lc: Eval[C])(f: (B, Eval[C]) => Eval[C]): Eval[C] = eab match {
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
   * scala> import cats.implicits._
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
  final def combine(that: Either[A, B])(implicit B: Semigroup[B]): Either[A, B] = eab match {
    case left @ Left(_) => left
    case Right(b1) => that match {
      case left @ Left(_) => left
      case Right(b2) => Right(B.combine(b1, b2))
    }
  }

  def show(implicit A: Show[A], B: Show[B]): String = eab match {
    case Left(a)  => s"Left(${A.show(a)})"
    case Right(b) => s"Right(${B.show(b)})"
  }

  def ap[C](that: Either[A, B => C]): Either[A, C] = (new EitherOps(that)).flatMap(this.map)

  /**
   * Convert a `scala.util.Either` into a [[cats.data.Xor]].
   *
   * Example:
   * {{{
   * scala> import cats.data.Xor
   * scala> import cats.implicits._
   *
   * scala> val i: Either[String, Int] = Right(3)
   * scala> i.toXor
   * res0: Xor[String, Int] = Right(3)
   *
   * scala> val s: Either[String, Int] = Left("error!")
   * scala> s.toXor
   * res0: Xor[String, Int] = Left(error!)
   * }}}
   */
  def toXor: A Xor B = Xor.fromEither(eab)

  /**
   * Transform the `Either` into a [[cats.data.EitherT]] while lifting it into the specified Applicative.
   *
   * {{{
   * scala> import cats.implicits._
   * scala> val e: Either[String, Int] = Right(3)
   * scala> e.toEitherT[Option]
   * res0: cats.data.EitherT[Option, String, Int] = EitherT(Some(Right(3)))
   * }}}
   */
  def toEitherT[F[_]: Applicative]: EitherT[F, A, B] = EitherT.fromEither(eab)
}

final class EitherObjectOps(val either: Either.type) extends AnyVal { // scalastyle:off ensure.single.space.after.token
  def left[A, B](a: A): Either[A, B] = Left(a)

  def right[A, B](b: B): Either[A, B] = Right(b)

  /**
   * Evaluates the specified block, catching exceptions of the specified type and returning them on the left side of
   * the resulting `Either`. Uncaught exceptions are propagated.
   *
   * For example:
   * {{{
   * scala> import cats.implicits._ // get syntax for Either
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
      case scala.util.control.NonFatal(t) => left(t)
    }

  /**
   * Converts a `Try[A]` to a `Throwable Xor A`.
   */
  def fromTry[A](t: Try[A]): Either[Throwable, A] =
    t match {
      case Failure(e) => left(e)
      case Success(v) => right(v)
    }

  /**
   * Converts an `Option[B]` to an `A Xor B`, where the provided `ifNone` values is returned on
   * the left of the `Xor` when the specified `Option` is `None`.
   */
  def fromOption[A, B](o: Option[B], ifNone: => A): Either[A, B] = o match {
    case None    => left[A, B](ifNone)
    case Some(a) => right(a)
  }
}

final class CatchOnlyPartiallyApplied[T] private[syntax] {
  def apply[A](f: => A)(implicit CT: ClassTag[T], NT: NotNull[T]): Either[T, A] =
    try {
      Right(f)
    } catch {
      case t if CT.runtimeClass.isInstance(t) =>
        Left(t.asInstanceOf[T])
    }
}

final class LeftOps[A, B](val left: Left[A, B]) extends AnyVal {
  /** Cast the right type parameter of the `Left`. */
  def rightCast[C]: Either[A, C] = left.asInstanceOf[Either[A, C]]
}

final class RightOps[A, B](val right: Right[A, B]) extends AnyVal {
  /** Cast the left type parameter of the `Right`. */
  def leftCast[C]: Either[C, B] = right.asInstanceOf[Either[C, B]]
}

/** Convenience methods to use `Either` syntax inside `Either` syntax definitions. */
private[cats] object EitherUtil {
  def leftCast[A, B, C](r: Right[A, B]): Either[C, B] = new RightOps(r).leftCast[C]

  def rightCast[A, B, C](l: Left[A, B]): Either[A, C] = new LeftOps(l).rightCast[C]
}
