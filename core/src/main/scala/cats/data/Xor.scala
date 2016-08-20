package cats
package data

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/** Represents a right-biased disjunction that is either an `A` or a `B`.
 *
 * An instance of `A [[Xor]] B` is either a `[[Xor.Left Left]][A]` or a `[[Xor.Right Right]][B]`.
 *
 * A common use of [[Xor]] is to explicitly represent the possibility of failure in a result as opposed to
 * throwing an exception.  By convention, [[Xor.Left Left]] is used for errors and [[Xor.Right Right]] is reserved for successes.
 * For example, a function that attempts to parse an integer from a string may have a return type of
 * `NumberFormatException [[Xor]] Int`. However, since there is no need to actually throw an exception, the type (`A`)
 * chosen for the "left" could be any type representing an error and has no need to actually extend `Exception`.
 *
 * `A [[Xor]] B` is isomorphic to `scala.Either[A, B]`, but [[Xor]] is right-biased, so methods such as `map` and
 * `flatMap` apply only in the context of the "right" case. This right bias makes [[Xor]] more convenient to use
 * than `scala.Either` in a monadic context. Methods such as `swap`, and `leftMap` provide functionality
 * that `scala.Either` exposes through left projections.
 *
 * Some additional [[Xor]] methods can be found in [[Xor.XorOps XorOps]]. These methods are not defined on [[Xor]] itself because
 * [[Xor]] is covariant in its types `A` and `B`.
 */
sealed abstract class Xor[+A, +B] extends Product with Serializable {

  def fold[C](fa: A => C, fb: B => C): C = this match {
    case Xor.Left(a) => fa(a)
    case Xor.Right(b) => fb(b)
  }

  def isLeft: Boolean = fold(_ => true, _ => false)

  def isRight: Boolean = fold(_ => false, _ => true)

  def swap: B Xor A = fold(Xor.right, Xor.left)

  def foreach(f: B => Unit): Unit = fold(_ => (), f)

  def getOrElse[BB >: B](default: => BB): BB = fold(_ => default, identity)

  def orElse[C, BB >: B](fallback: => C Xor BB): C Xor BB = this match {
    case Xor.Left(_)      => fallback
    case r @ Xor.Right(_) => r
  }

  def recover[BB >: B](pf: PartialFunction[A, BB]): A Xor BB = this match {
    case Xor.Left(a) if pf.isDefinedAt(a) => Xor.right(pf(a))
    case _                                => this
  }

  def recoverWith[AA >: A, BB >: B](pf: PartialFunction[A, AA Xor BB]): AA Xor BB = this match {
    case Xor.Left(a) if pf.isDefinedAt(a) => pf(a)
    case _                                => this
  }

  def valueOr[BB >: B](f: A => BB): BB = fold(f, identity)

  def forall(f: B => Boolean): Boolean = fold(_ => true, f)

  def exists(f: B => Boolean): Boolean = fold(_ => false, f)

  def ensure[AA >: A](onFailure: => AA)(f: B => Boolean): AA Xor B =
    fold(_ => this, b => if (f(b)) this else Xor.Left(onFailure))

  def toIor: A Ior B = fold(Ior.left, Ior.right)

  def toEither: Either[A, B] = fold(Left(_), Right(_))

  def toOption: Option[B] = fold(_ => None, Some(_))

  def toList: List[B] = fold(_ => Nil, _ :: Nil)

  def toTry(implicit ev: A <:< Throwable): Try[B] = fold(a => Failure(ev(a)), Success(_))

  def toValidated: Validated[A, B] = fold(Validated.Invalid.apply, Validated.Valid.apply)

  /** Returns a [[ValidatedNel]] representation of this disjunction with the `Left` value
   * as a single element on the `Invalid` side of the [[NonEmptyList]]. */
  def toValidatedNel[AA >: A]: ValidatedNel[AA, B] = fold(Validated.invalidNel, Validated.valid)

  def withValidated[AA, BB](f: Validated[A, B] => Validated[AA, BB]): AA Xor BB =
    f(toValidated).toXor

  def to[F[_], BB >: B](implicit F: Alternative[F]): F[BB] =
    fold(_ => F.empty, F.pure)

  def bimap[C, D](fa: A => C, fb: B => D): C Xor D = this match {
    case Xor.Left(a) => Xor.Left(fa(a))
    case Xor.Right(b) => Xor.Right(fb(b))
  }

  def map[D](f: B => D): A Xor D = this match {
    case l @ Xor.Left(_) => l
    case Xor.Right(b)    => Xor.Right(f(b))
  }

  def map2Eval[AA >: A, C, Z](fc: Eval[AA Xor C])(f: (B, C) => Z): Eval[AA Xor Z] =
    this match {
      case l @ Xor.Left(_) => Now(l)
      case Xor.Right(b) => fc.map(_.map(f(b, _)))
    }

  def leftMap[C](f: A => C): C Xor B = this match {
    case Xor.Left(a)      => Xor.Left(f(a))
    case r @ Xor.Right(_) => r
  }

  def flatMap[AA >: A, D](f: B => AA Xor D): AA Xor D = this match {
    case l @ Xor.Left(_) => l
    case Xor.Right(b) => f(b)
  }

  def compare[AA >: A, BB >: B](that: AA Xor BB)(implicit AA: Order[AA], BB: Order[BB]): Int = fold(
    a => that.fold(AA.compare(a, _), _ => -1),
    b => that.fold(_ => 1, BB.compare(b, _))
  )

  def partialCompare[AA >: A, BB >: B](that: AA Xor BB)(implicit AA: PartialOrder[AA], BB: PartialOrder[BB]): Double = fold(
    a => that.fold(AA.partialCompare(a, _), _ => -1),
    b => that.fold(_ => 1, BB.partialCompare(b, _))
  )

  def ===[AA >: A, BB >: B](that: AA Xor BB)(implicit AA: Eq[AA], BB: Eq[BB]): Boolean = fold(
    a => that.fold(AA.eqv(a, _), _ => false),
    b => that.fold(_ => false, BB.eqv(b, _))
  )

  def traverse[F[_], AA >: A, D](f: B => F[D])(implicit F: Applicative[F]): F[AA Xor D] = this match {
    case l @ Xor.Left(_) => F.pure(l)
    case Xor.Right(b) => F.map(f(b))(Xor.right _)
  }

  def foldLeft[C](c: C)(f: (C, B) => C): C = fold(_ => c, f(c, _))

  def foldRight[C](lc: Eval[C])(f: (B, Eval[C]) => Eval[C]): Eval[C] =
    fold(_ => lc, b => f(b, lc))

  def merge[AA >: A](implicit ev: B <:< AA): AA = fold(identity, ev.apply)

  /**
   * Combine with another `Xor` value.
   *
   * If this `Xor` is a `Left` then it will be returned as-is.
   * If this `Xor` is a `Right` and `that` `Xor` is a left, then `that` will be
   * returned.
   * If both `Xor`s are `Right`s, then the `Semigroup[BB]` instance will be used
   * to combine both values and return them as a `Right`.
   * Note: If both `Xor`s are `Left`s then their values are not combined. Use
   * `Validated` if you prefer to combine `Left` values.
   *
   * Examples:
   * {{{
   * scala> import cats.data.Xor
   * scala> import cats.implicits._
   * scala> val l1: Xor[String, Int] = Xor.left("error 1")
   * scala> val l2: Xor[String, Int] = Xor.left("error 2")
   * scala> val r3: Xor[String, Int] = Xor.right(3)
   * scala> val r4: Xor[String, Int] = Xor.right(4)
   *
   * scala> l1 combine l2
   * res0: Xor[String, Int] = Left(error 1)
   *
   * scala> l1 combine r3
   * res1: Xor[String, Int] = Left(error 1)
   *
   * scala> r3 combine l1
   * res2: Xor[String, Int] = Left(error 1)
   *
   * scala> r3 combine r4
   * res3: Xor[String, Int] = Right(7)
   * }}}
   */
  final def combine[AA >: A, BB >: B](that: AA Xor BB)(implicit BB: Semigroup[BB]): AA Xor BB = this match {
    case left @ Xor.Left(_) => left
    case Xor.Right(b1) => that match {
      case left @ Xor.Left(_) => left
      case Xor.Right(b2) => Xor.Right(BB.combine(b1, b2))
    }
  }

  def show[AA >: A, BB >: B](implicit AA: Show[AA], BB: Show[BB]): String = fold(
    a => s"Xor.Left(${AA.show(a)})",
    b => s"Xor.Right(${BB.show(b)})"
  )

  def ap[AA >: A, BB >: B, C](that: AA Xor (BB => C)): AA Xor C = that.flatMap(this.map)
}

object Xor extends XorInstances with XorFunctions {
  final case class Left[+A](a: A) extends (A Xor Nothing)
  final case class Right[+B](b: B) extends (Nothing Xor B)

  final implicit class XorOps[A, B](val value: A Xor B) extends AnyVal {
    /**
     * Transform the `Xor` into a [[XorT]] while lifting it into the specified Applicative.
     *
     * {{{
     * scala> import cats.implicits._
     * scala> val x: Xor[String, Int] = Xor.right(3)
     * scala> x.toXorT[Option]
     * res0: cats.data.XorT[Option, String, Int] = XorT(Some(Right(3)))
     * }}}
     */
    def toXorT[F[_]: Applicative]: XorT[F, A, B] = XorT.fromXor(value)
  }
}

private[data] sealed abstract class XorInstances extends XorInstances1 {
  implicit def catsDataOrderForXor[A: Order, B: Order]: Order[A Xor B] =
    new Order[A Xor B] {
      def compare(x: A Xor B, y: A Xor B): Int = x compare y
      override def partialCompare(x: A Xor B, y: A Xor B): Double = x partialCompare y
      override def eqv(x: A Xor B, y: A Xor B): Boolean = x === y
    }

  implicit def catsDataShowForXor[A, B](implicit A: Show[A], B: Show[B]): Show[A Xor B] =
    new Show[A Xor B] {
      def show(f: A Xor B): String = f.show
    }

  implicit def catsDataMonoidForXor[A, B](implicit B: Monoid[B]): Monoid[A Xor B] =
    new Monoid[A Xor B] {
      def empty: A Xor B = Xor.Right(B.empty)
      def combine(x: A Xor B, y: A Xor B): A Xor B = x combine y
    }

  implicit def catsDataSemigroupKForXor[L]: SemigroupK[Xor[L, ?]] =
    new SemigroupK[Xor[L, ?]] {
      def combineK[A](x: Xor[L, A], y: Xor[L, A]): Xor[L, A] = x match {
        case Xor.Left(_) => y
        case Xor.Right(_) => x
      }
    }

  implicit val catsDataBitraverseForXor: Bitraverse[Xor] =
    new Bitraverse[Xor] {
      def bitraverse[G[_], A, B, C, D](fab: Xor[A, B])(f: A => G[C], g: B => G[D])(implicit G: Applicative[G]): G[Xor[C, D]] =
        fab match {
          case Xor.Left(a) => G.map(f(a))(Xor.left)
          case Xor.Right(b) => G.map(g(b))(Xor.right)
        }

      def bifoldLeft[A, B, C](fab: Xor[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
        fab match {
          case Xor.Left(a) => f(c, a)
          case Xor.Right(b) => g(c, b)
        }

      def bifoldRight[A, B, C](fab: Xor[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
        fab match {
          case Xor.Left(a) => f(a, c)
          case Xor.Right(b) => g(b, c)
        }
    }

  implicit def catsDataInstancesForXor[A]: Traverse[A Xor ?] with Monad[A Xor ?] with MonadError[Xor[A, ?], A] with RecursiveTailRecM[A Xor ?] =
    new Traverse[A Xor ?] with Monad[A Xor ?] with MonadError[Xor[A, ?], A] with RecursiveTailRecM[A Xor ?] {
      def traverse[F[_]: Applicative, B, C](fa: A Xor B)(f: B => F[C]): F[A Xor C] = fa.traverse(f)
      def foldLeft[B, C](fa: A Xor B, c: C)(f: (C, B) => C): C = fa.foldLeft(c)(f)
      def foldRight[B, C](fa: A Xor B, lc: Eval[C])(f: (B, Eval[C]) => Eval[C]): Eval[C] = fa.foldRight(lc)(f)
      def flatMap[B, C](fa: A Xor B)(f: B => A Xor C): A Xor C = fa.flatMap(f)
      override def ap[B, C](x: A Xor (B => C))(y: A Xor B): A Xor C = y.ap(x)
      def pure[B](b: B): A Xor B = Xor.right(b)
      @tailrec def tailRecM[B, C](b: B)(f: B => A Xor Either[B, C]): A Xor C =
        f(b) match {
          case Xor.Left(a) => Xor.Left(a)
          case Xor.Right(Left(b1)) => tailRecM(b1)(f)
          case Xor.Right(Right(c)) => Xor.Right(c)
        }
      def handleErrorWith[B](fea: Xor[A, B])(f: A => Xor[A, B]): Xor[A, B] =
        fea match {
          case Xor.Left(e) => f(e)
          case r @ Xor.Right(_) => r
        }
      def raiseError[B](e: A): Xor[A, B] = Xor.left(e)
      override def map[B, C](fa: A Xor B)(f: B => C): A Xor C = fa.map(f)
      override def map2Eval[B, C, Z](fb: A Xor B, fc: Eval[A Xor C])(f: (B, C) => Z): Eval[A Xor Z] =
        fb.map2Eval(fc)(f)
      override def attempt[B](fab: A Xor B): A Xor (Xor[A, B]) = Xor.right(fab)
      override def recover[B](fab: A Xor B)(pf: PartialFunction[A, B]): A Xor B =
        fab recover pf
      override def recoverWith[B](fab: A Xor B)(pf: PartialFunction[A, A Xor B]): A Xor B =
        fab recoverWith pf
      override def ensure[B](fab: A Xor B)(error: => A)(predicate: B => Boolean): A Xor B =
        fab.ensure(error)(predicate)
    }
}

private[data] sealed abstract class XorInstances1 extends XorInstances2 {

  implicit def catsDataSemigroupForXor[A, B](implicit B: Semigroup[B]): Semigroup[A Xor B] =
    new Semigroup[A Xor B] {
      def combine(x: A Xor B, y: A Xor B): A Xor B = x combine y
    }

  implicit def catsDataPartialOrderForXor[A: PartialOrder, B: PartialOrder]: PartialOrder[A Xor B] = new PartialOrder[A Xor B] {
    def partialCompare(x: A Xor B, y: A Xor B): Double = x partialCompare y
    override def eqv(x: A Xor B, y: A Xor B): Boolean = x === y
  }
}

private[data] sealed abstract class XorInstances2 {
  implicit def catsDataEqForXor[A: Eq, B: Eq]: Eq[A Xor B] =
    new Eq[A Xor B] {
      def eqv(x: A Xor B, y: A Xor B): Boolean = x === y
    }
}

trait XorFunctions {
  def left[A, B](a: A): A Xor B = Xor.Left(a)

  def right[A, B](b: B): A Xor B = Xor.Right(b)

  /**
   * Evaluates the specified block, catching exceptions of the specified type and returning them on the left side of
   * the resulting `Xor`. Uncaught exceptions are propagated.
   *
   * For example:
   * {{{
   * scala> Xor.catchOnly[NumberFormatException] { "foo".toInt }
   * res0: Xor[NumberFormatException, Int] = Left(java.lang.NumberFormatException: For input string: "foo")
   * }}}
   *
   * This method and its usage of [[NotNull]] are inspired by and derived from
   * the `fromTryCatchThrowable` method [[https://github.com/scalaz/scalaz/pull/746/files contributed]]
   * to Scalaz by Brian McKenna.
   */
  def catchOnly[T >: Null <: Throwable]: CatchOnlyPartiallyApplied[T] =
    new CatchOnlyPartiallyApplied[T]

  final class CatchOnlyPartiallyApplied[T] private[XorFunctions] {
    def apply[A](f: => A)(implicit CT: ClassTag[T], NT: NotNull[T]): T Xor A =
      try {
        right(f)
      } catch {
        case t if CT.runtimeClass.isInstance(t) =>
          left(t.asInstanceOf[T])
      }
  }

  def catchNonFatal[A](f: => A): Throwable Xor A =
    try {
      right(f)
    } catch {
      case scala.util.control.NonFatal(t) => left(t)
    }

  /**
   * Converts a `Try[A]` to a `Throwable Xor A`.
   */
  def fromTry[A](t: Try[A]): Throwable Xor A =
    t match {
      case Failure(e) => left(e)
      case Success(v) => right(v)
    }

  /**
   * Converts an `Either[A, B]` to an `A Xor B`.
   */
  def fromEither[A, B](e: Either[A, B]): A Xor B =
    e.fold(left, right)

  /**
   * Converts an `Option[B]` to an `A Xor B`, where the provided `ifNone` values is returned on
   * the left of the `Xor` when the specified `Option` is `None`.
   */
  def fromOption[A, B](o: Option[B], ifNone: => A): A Xor B =
    o.fold(left[A, B](ifNone))(right)
}
