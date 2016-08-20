package cats
package data

import cats.data.Validated.{Invalid, Valid}

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

sealed abstract class Validated[+E, +A] extends Product with Serializable {

  def fold[B](fe: E => B, fa: A => B): B =
    this match {
      case Invalid(e) => fe(e)
      case Valid(a) => fa(a)
    }

  def isValid: Boolean = fold(_ => false, _ => true)
  def isInvalid: Boolean = fold(_ => true, _ => false)

  /**
   * Run the side-effecting function on the value if it is Valid
   */
  def foreach(f: A => Unit): Unit = fold(_ => (), f)

  /**
   * Return the Valid value, or the default if Invalid
   */
  def getOrElse[B >: A](default: => B): B = fold(_ => default, identity)

  /**
    * Return the Valid value, or the result of f if Invalid
    */
  def valueOr[B >: A](f: E => B): B = fold(f, identity)

  /**
   * Is this Valid and matching the given predicate
   */
  def exists(predicate: A => Boolean): Boolean = fold(_ => false, predicate)

  /**
   * Is this Invalid or matching the predicate
   */
  def forall(f: A => Boolean): Boolean = fold(_ => true, f)

  /**
   * Return this if it is Valid, or else fall back to the given default.
   */
  def orElse[EE, AA >: A](default: => Validated[EE, AA]): Validated[EE, AA] =
    this match {
      case v @ Valid(_) => v
      case Invalid(_) => default
    }

  /**
   * Converts the value to an Either[E, A]
   */
  def toEither: Either[E, A] = fold(Left.apply, Right.apply)

  /**
   * Returns Valid values wrapped in Some, and None for Invalid values
   */
  def toOption: Option[A] = fold(_ => None, Some.apply)

  /**
   * Convert this value to a single element List if it is Valid,
   * otherwise return an empty List
   */
  def toList: List[A] = fold(_ => Nil, List(_))

  /** Lift the Invalid value into a NonEmptyList. */
  def toValidatedNel[EE >: E, AA >: A]: ValidatedNel[EE, AA] =
    this match {
      case v @ Valid(_) => v
      case Invalid(e)   => Validated.invalidNel(e)
    }

  /**
   * Convert this value to RightOr if Valid or LeftOr if Invalid
   */
  def toXor: Xor[E, A] = fold(Xor.Left.apply, Xor.Right.apply)

  /**
   * Convert to an Xor, apply a function, convert back.  This is handy
   * when you want to use the Monadic properties of the Xor type.
   */
  def withXor[EE, B](f: Xor[E, A] => Xor[EE, B]): Validated[EE, B] =
    Validated.fromXor(f(toXor))

  /**
   * Validated is a [[functor.Bifunctor]], this method applies one of the
   * given functions.
   */
  def bimap[EE, AA](fe: E => EE, fa: A => AA): Validated[EE, AA] =
    fold(fe andThen Invalid.apply,
         fa andThen Valid.apply)

  def compare[EE >: E, AA >: A](that: Validated[EE, AA])(implicit EE: Order[EE], AA: Order[AA]): Int = fold(
    a => that.fold(EE.compare(a, _), _ => -1),
    b => that.fold(_ => 1, AA.compare(b, _))
  )

  def partialCompare[EE >: E, AA >: A](that: Validated[EE, AA])(implicit EE: PartialOrder[EE], AA: PartialOrder[AA]): Double = fold(
    a => that.fold(EE.partialCompare(a, _), _ => -1),
    b => that.fold(_ => 1, AA.partialCompare(b, _))
  )

  def ===[EE >: E, AA >: A](that: Validated[EE, AA])(implicit EE: Eq[EE], AA: Eq[AA]): Boolean = fold(
    a => that.fold(EE.eqv(a, _), _ => false),
    b => that.fold(_ => false, AA.eqv(b, _))
  )

  /**
   * From Apply:
   * if both the function and this value are Valid, apply the function
   */
  def ap[EE >: E, B](f: Validated[EE, A => B])(implicit EE: Semigroup[EE]): Validated[EE, B] =
    (this, f) match {
      case (Valid(a), Valid(f)) => Valid(f(a))
      case (Invalid(e1), Invalid(e2)) => Invalid(EE.combine(e2, e1))
      case (e@Invalid(_), _) => e
      case (_, e@Invalid(_)) => e
    }

  /**
   * From Product
   */
  def product[EE >: E, B](fb: Validated[EE, B])(implicit EE: Semigroup[EE]): Validated[EE, (A, B)] =
    (this, fb) match {
      case (Valid(a), Valid(b)) => Valid((a, b))
      case (Invalid(e1), Invalid(e2)) => Invalid(EE.combine(e1, e2))
      case (e @ Invalid(_), _) => e
      case (_, e @ Invalid(_)) => e
    }

  /**
   * Apply a function to a Valid value, returning a new Valid value
   */
  def map[B](f: A => B): Validated[E, B] = bimap(identity, f)

  /**
   * Apply a function to an Invalid value, returning a new Invalid value.
   * Or, if the original valid was Valid, return it.
   */
  def leftMap[EE](f: E => EE): Validated[EE, A] = bimap(f, identity)

  /**
   * When Valid, apply the function, marking the result as valid
   * inside the Applicative's context,
   * when Invalid, lift the Error into the Applicative's context
   */
  def traverse[F[_], EE >: E, B](f: A => F[B])(implicit F: Applicative[F]): F[Validated[EE, B]] =
    fold(e => F.pure(Invalid(e)),
         a => F.map(f(a))(Valid.apply))

  /**
   * apply the given function to the value with the given B when
   * valid, otherwise return the given B
   */
  def foldLeft[B](b: B)(f: (B, A) => B): B =
    fold(_ => b, f(b, _))

  /**
   * Lazily-apply the given function to the value with the given B
   * when valid, otherwise return the given B.
   */
  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    fold(_ => lb, a => f(a, lb))

  def show[EE >: E, AA >: A](implicit EE: Show[EE], AA: Show[AA]): String =
    fold(e => s"Invalid(${EE.show(e)})",
         a => s"Valid(${AA.show(a)})")

  /**
   * Apply a function (that returns a `Validated`) in the valid case.
   * Otherwise return the original `Validated`.
   *
   * This allows "chained" validation: the output of one validation can be fed
   * into another validation function.
   *
   * This function is similar to `flatMap` on `Either`. It's not called `flatMap`,
   * because by Cats convention, `flatMap` is a monadic bind that is consistent
   * with `ap`. This method is not consistent with [[ap]] (or other
   * `Apply`-based methods), because it has "fail-fast" behavior as opposed to
   * accumulating validation failures.
   */
  def andThen[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] =
    this match {
      case Valid(a) => f(a)
      case i @ Invalid(_) => i
    }

  /**
   * Combine this `Validated` with another `Validated`, using the `Semigroup`
   * instances of the underlying `E` and `A` instances. The resultant `Validated`
   * will be `Valid`, if, and only if, both this `Validated` instance and the
   * supplied `Validated` instance are also `Valid`.
   */
  def combine[EE >: E, AA >: A](that: Validated[EE, AA])(implicit EE: Semigroup[EE], AA: Semigroup[AA]): Validated[EE, AA] =
    (this, that) match {
      case (Valid(a), Valid(b)) => Valid(AA.combine(a, b))
      case (Invalid(a), Invalid(b)) => Invalid(EE.combine(a, b))
      case (Invalid(_), _) => this
      case _ => that
    }

  def swap: Validated[A, E] = this match {
    case Valid(a) => Invalid(a)
    case Invalid(e) => Valid(e)
  }

  def merge[EE >: E](implicit ev: A <:< EE): EE = fold(identity, ev.apply)

  /**
   * Ensure that a successful result passes the given predicate,
   * falling back to an Invalid of `onFailure` if the predicate
   * returns false.
   *
   * For example:
   * {{{
   * scala> Validated.valid("").ensure(new IllegalArgumentException("Must not be empty"))(_.nonEmpty)
   * res0: Validated[IllegalArgumentException, String] = Invalid(java.lang.IllegalArgumentException: Must not be empty)
   * }}}
   */
  def ensure[EE >: E](onFailure: => EE)(f: A => Boolean): Validated[EE, A] =
    fold(_ => this, a => if (f(a)) this else Validated.invalid(onFailure))
}

object Validated extends ValidatedInstances with ValidatedFunctions{
  final case class Valid[+A](a: A) extends Validated[Nothing, A]
  final case class Invalid[+E](e: E) extends Validated[E, Nothing]
}

private[data] sealed abstract class ValidatedInstances extends ValidatedInstances1 {

  implicit def catsDataSemigroupKForValidated[A](implicit A: Semigroup[A]): SemigroupK[Validated[A, ?]] =
    new SemigroupK[Validated[A, ?]] {
      def combineK[B](x: Validated[A, B], y: Validated[A, B]): Validated[A, B] = x match {
        case v @ Valid(_) => v
        case Invalid(ix) => y match {
          case Invalid(iy) => Invalid(A.combine(ix, iy))
          case v @ Valid(_) => v
        }
      }
    }

  implicit def catsDataMonoidForValidated[A, B](implicit A: Semigroup[A], B: Monoid[B]): Monoid[Validated[A, B]] = new Monoid[Validated[A, B]] {
    def empty: Validated[A, B] = Valid(B.empty)
    def combine(x: Validated[A, B], y: Validated[A, B]): Validated[A, B] = x combine y
  }

  implicit def catsDataOrderForValidated[A: Order, B: Order]: Order[Validated[A, B]] = new Order[Validated[A, B]] {
    def compare(x: Validated[A, B], y: Validated[A, B]): Int = x compare y
    override def partialCompare(x: Validated[A, B], y: Validated[A, B]): Double = x partialCompare y
    override def eqv(x: Validated[A, B], y: Validated[A, B]): Boolean = x === y
  }

  implicit def catsDataShowForValidated[A, B](implicit A: Show[A], B: Show[B]): Show[Validated[A, B]] = new Show[Validated[A, B]] {
    def show(f: Validated[A, B]): String = f.show
  }

  implicit val catsDataBitraverseForValidated: Bitraverse[Validated] =
    new Bitraverse[Validated] {
      def bitraverse[G[_], A, B, C, D](fab: Validated[A, B])(f: A => G[C], g: B => G[D])(implicit G: Applicative[G]): G[Validated[C, D]] =
        fab match {
          case Invalid(a) => G.map(f(a))(Validated.invalid)
          case Valid(b) => G.map(g(b))(Validated.valid)
        }

      def bifoldLeft[A, B, C](fab: Validated[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
        fab match {
          case Invalid(a) => f(c, a)
          case Valid(b) => g(c, b)
        }

      def bifoldRight[A, B, C](fab: Validated[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
        fab match {
          case Invalid(a) => f(a, c)
          case Valid(b) => g(b, c)
        }

      override def bimap[A, B, C, D](fab: Validated[A, B])(f: A => C, g: B => D): Validated[C, D] =
        fab.bimap(f, g)

      override def leftMap[A, B, C](fab: Validated[A, B])(f: A => C): Validated[C, B] =
        fab.leftMap(f)
    }

  implicit def catsDataInstancesForValidated[E](implicit E: Semigroup[E]): Traverse[Validated[E, ?]] with ApplicativeError[Validated[E, ?], E] =
    new Traverse[Validated[E, ?]] with ApplicativeError[Validated[E, ?], E] {
      def traverse[F[_]: Applicative, A, B](fa: Validated[E, A])(f: A => F[B]): F[Validated[E, B]] =
        fa.traverse(f)

      def foldLeft[A, B](fa: Validated[E, A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: Validated[E, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)

      def pure[A](a: A): Validated[E, A] =
        Validated.valid(a)

      override def map[A, B](fa: Validated[E, A])(f: A => B): Validated[E, B] =
        fa.map(f)

      def ap[A, B](f: Validated[E, A => B])(fa: Validated[E, A]): Validated[E, B] =
        fa.ap(f)(E)

      override def product[A, B](fa: Validated[E, A], fb: Validated[E, B]): Validated[E, (A, B)] =
        fa.product(fb)(E)

      def handleErrorWith[A](fa: Validated[E, A])(f: E => Validated[E, A]): Validated[E, A] =
        fa match {
          case Validated.Invalid(e) => f(e)
          case v @ Validated.Valid(_) => v
        }
      def raiseError[A](e: E): Validated[E, A] = Validated.Invalid(e)
    }
}

private[data] sealed abstract class ValidatedInstances1 extends ValidatedInstances2 {

  implicit def catsDataSemigroupForValidated[A, B](implicit A: Semigroup[A], B: Semigroup[B]): Semigroup[Validated[A, B]] =
    new Semigroup[Validated[A, B]] {
      def combine(x: Validated[A, B], y: Validated[A, B]): Validated[A, B] = x combine y
    }

  implicit def catsDataPartialOrderForValidated[A: PartialOrder, B: PartialOrder]: PartialOrder[Validated[A, B]] =
    new PartialOrder[Validated[A, B]] {
      def partialCompare(x: Validated[A, B], y: Validated[A, B]): Double = x partialCompare y
      override def eqv(x: Validated[A, B], y: Validated[A, B]): Boolean = x === y
    }
}

private[data] sealed abstract class ValidatedInstances2 {
  implicit def catsDataEqForValidated[A: Eq, B: Eq]: Eq[Validated[A, B]] =
    new Eq[Validated[A, B]] {
      def eqv(x: Validated[A, B], y: Validated[A, B]): Boolean = x === y
    }
}

trait ValidatedFunctions {
  def invalid[A, B](a: A): Validated[A, B] = Validated.Invalid(a)

  def invalidNel[A, B](a: A): ValidatedNel[A, B] = Validated.Invalid(NonEmptyList(a, Nil))

  def valid[A, B](b: B): Validated[A, B] = Validated.Valid(b)

  /**
   * Evaluates the specified block, catching exceptions of the specified type and returning them on the invalid side of
   * the resulting `Validated`. Uncaught exceptions are propagated.
   *
   * For example:
   * {{{
   * scala> Validated.catchOnly[NumberFormatException] { "foo".toInt }
   * res0: Validated[NumberFormatException, Int] = Invalid(java.lang.NumberFormatException: For input string: "foo")
   * }}}
   *
   * This method and its usage of [[NotNull]] are inspired by and derived from
   * the `fromTryCatchThrowable` method [[https://github.com/scalaz/scalaz/pull/746/files contributed]]
   * to Scalaz by Brian McKenna.
   */
  def catchOnly[T >: Null <: Throwable]: CatchOnlyPartiallyApplied[T] = new CatchOnlyPartiallyApplied[T]

  final class CatchOnlyPartiallyApplied[T] private[ValidatedFunctions] {
    def apply[A](f: => A)(implicit T: ClassTag[T], NT: NotNull[T]): Validated[T, A] =
      try {
        valid(f)
      } catch {
        case t if T.runtimeClass.isInstance(t) =>
          invalid(t.asInstanceOf[T])
      }
  }

  def catchNonFatal[A](f: => A): Validated[Throwable, A] =
    try {
      valid(f)
    } catch {
      case scala.util.control.NonFatal(t) => invalid(t)
    }

  /**
   * Converts a `Try[A]` to a `Validated[Throwable, A]`.
   */
  def fromTry[A](t: Try[A]): Validated[Throwable, A] = t match {
    case Failure(e) => invalid(e)
    case Success(v) => valid(v)
  }

  /**
   * Converts an `Either[A, B]` to an `Validated[A, B]`.
   */
  def fromEither[A, B](e: Either[A, B]): Validated[A, B] = e.fold(invalid, valid)

  /**
   * Converts an `Xor[A, B]` to an `Validated[A, B]`.
   */
  def fromXor[A, B](e: Xor[A, B]): Validated[A, B] = e.fold(invalid, valid)

  /**
   * Converts an `Option[B]` to an `Validated[A, B]`, where the provided `ifNone` values is returned on
   * the invalid of the `Validated` when the specified `Option` is `None`.
   */
  def fromOption[A, B](o: Option[B], ifNone: => A): Validated[A, B] = o.fold(invalid[A, B](ifNone))(valid)
}
