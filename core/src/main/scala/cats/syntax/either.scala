package cats
package syntax

import cats.data.{EitherT, Ior, Validated, ValidatedNel, Xor}
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

trait EitherSyntax {
  implicit def catsSyntaxEither[A, B](eab: Either[A, B]): EitherOps[A, B] = new EitherOps(eab)

  implicit def catsSyntaxEitherObject(either: Either.type): EitherObjectOps = new EitherObjectOps(either)
}

final class EitherOps[A, B](val eab: Either[A, B]) extends AnyVal {
  def foreach(f: B => Unit): Unit = eab.fold(_ => (), f)

  def getOrElse[BB >: B](default: => BB): BB = eab.fold(_ => default, identity)

  def orElse[C, BB >: B](fallback: => Either[C, BB]): Either[C, BB]= eab match {
    case Left(_)      => fallback
    case r @ Right(_) => r.asInstanceOf[Either[C, BB]]
  }

  def recover[BB >: B](pf: PartialFunction[A, BB]): Either[A, BB] = eab match {
    case Left(a) if pf.isDefinedAt(a) => Right(pf(a))
    case _                            => eab
  }

  def recoverWith[AA >: A, BB >: B](pf: PartialFunction[A, Either[AA, BB]]): Either[AA, BB] = eab match {
    case Left(a) if pf.isDefinedAt(a) => pf(a)
    case _                            => eab
  }

  def valueOr[BB >: B](f: A => BB): BB = eab.fold(f, identity)

  def forall(f: B => Boolean): Boolean = eab.fold(_ => true, f)

  def exists(f: B => Boolean): Boolean = eab.fold(_ => false, f)

  def ensure[AA >: A](onFailure: => AA)(f: B => Boolean): Either[AA, B] =
    eab.fold(_ => eab, b => if (f(b)) eab else Left(onFailure))

  def toIor: A Ior B = eab.fold(Ior.left, Ior.right)

  def toOption: Option[B] = eab.fold(_ => None, Some(_))

  def toList: List[B] = eab.fold(_ => Nil, _ :: Nil)

  def toTry(implicit ev: A <:< Throwable): Try[B] = eab.fold(a => Failure(ev(a)), Success(_))

  def toValidated: Validated[A, B] = eab.fold(Validated.Invalid.apply, Validated.Valid.apply)

  /** Returns a [[ValidatedNel]] representation of this disjunction with the `Left` value
   * as a single element on the `Invalid` side of the [[NonEmptyList]]. */
  def toValidatedNel[AA >: A]: ValidatedNel[AA, B] = eab.fold(Validated.invalidNel, Validated.valid)

  def withValidated[AA, BB](f: Validated[A, B] => Validated[AA, BB]): Either[AA, BB] =
    f(toValidated).toEither

  def to[F[_], BB >: B](implicit F: Alternative[F]): F[BB] =
    eab.fold(_ => F.empty, F.pure)

  def bimap[C, D](fa: A => C, fb: B => D): Either[C, D] = eab match {
    case Left(a)  => Left(fa(a))
    case Right(b) => Right(fb(b))
  }

  def map[C](f: B => C): Either[A, C] = eab match {
    case l @ Left(_) => l.asInstanceOf[Either[A, C]]
    case Right(b)    => Right(f(b))
  }

  def map2Eval[AA >: A, C, Z](fc: Eval[Either[AA, C]])(f: (B, C) => Z): Eval[Either[AA, Z]] =
    eab match {
      case l @ Left(_) => Now(l.asInstanceOf[Either[AA, Z]])
      case Right(b)    => fc.map(either => new EitherOps(either).map(f(b, _)))
    }

  def leftMap[C](f: A => C): Either[C, B] = eab match {
    case Left(a)      => Left(f(a))
    case r @ Right(_) => r.asInstanceOf[Either[C, B]]
  }

  def flatMap[AA >: A, D](f: B => Either[AA, D]): Either[AA, D] = eab match {
    case l @ Left(_) => l.asInstanceOf[Either[AA, D]]
    case Right(b)    => f(b)
  }

  def compare[AA >: A, BB >: B](that: Either[AA, BB])(implicit AA: Order[AA], BB: Order[BB]): Int = eab.fold(
    a => that.fold(AA.compare(a, _), _ => -1),
    b => that.fold(_ => 1, BB.compare(b, _))
  )

  def partialCompare[AA >: A, BB >: B](that: Either[AA, BB])(implicit AA: PartialOrder[AA], BB: PartialOrder[BB]): Double =
    eab.fold(
      a => that.fold(AA.partialCompare(a, _), _ => -1),
      b => that.fold(_ => 1, BB.partialCompare(b, _))
    )

  def ===[AA >: A, BB >: B](that: Either[AA, BB])(implicit AA: Eq[AA], BB: Eq[BB]): Boolean = eab.fold(
    a => that.fold(AA.eqv(a, _), _ => false),
    b => that.fold(_ => false, BB.eqv(b, _))
  )

  def traverse[F[_], AA >: A, D](f: B => F[D])(implicit F: Applicative[F]): F[Either[AA, D]] = eab match {
    case l @ Left(_) => F.pure(l.asInstanceOf[Either[AA, D]])
    case Right(b) => F.map(f(b))(Right(_))
  }

  def foldLeft[C](c: C)(f: (C, B) => C): C = eab.fold(_ => c, f(c, _))

  def foldRight[C](lc: Eval[C])(f: (B, Eval[C]) => Eval[C]): Eval[C] =
    eab.fold(_ => lc, b => f(b, lc))

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
  final def combine[AA >: A, BB >: B](that: Either[AA, BB])(implicit BB: Semigroup[BB]): Either[AA, BB] = eab match {
    case left @ Left(_) => left
    case Right(b1) => that match {
      case left @ Left(_) => left
      case Right(b2) => Right(BB.combine(b1, b2))
    }
  }

  def show[AA >: A, BB >: B](implicit AA: Show[AA], BB: Show[BB]): String = eab.fold(
    a => s"Left(${AA.show(a)})",
    b => s"Right(${BB.show(b)})"
  )

  def ap[AA >: A, BB >: B, C](that: Either[AA, BB => C]): Either[AA, C] = (new EitherOps(that)).flatMap(this.map)

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
   * Transform the `Either` into a [[EitherT]] while lifting it into the specified Applicative.
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

final class EitherObjectOps(val either: Either.type) extends AnyVal {
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
  def fromOption[A, B](o: Option[B], ifNone: => A): Either[A, B] =
    o.fold(left[A, B](ifNone))(right)
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
