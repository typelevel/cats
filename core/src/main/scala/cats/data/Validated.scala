package cats
package data

import cats.data.Validated.{Invalid, Valid}
import cats.kernel.CommutativeSemigroup

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

sealed abstract class Validated[+E, +A] extends Product with Serializable {

  def fold[B](fe: E => B, fa: A => B): B =
    this match {
      case Invalid(e) => fe(e)
      case Valid(a) => fa(a)
    }

  def isValid: Boolean = this match {
    case Invalid(_) => false
    case _ => true
  }

  def isInvalid: Boolean = this match {
    case Invalid(_) => true
    case _ => false
  }

  /**
   * Run the side-effecting function on the value if it is Valid
   */
  def foreach(f: A => Unit): Unit = this match {
    case Valid(a) => f(a)
    case _ => ()
  }

  /**
   * Return the Valid value, or the default if Invalid
   */
  def getOrElse[B >: A](default: => B): B = this match {
    case Valid(a) => a
    case _ => default
  }

  /**
    * Return the Valid value, or the result of f if Invalid
    */
  def valueOr[B >: A](f: E => B): B = this match {
    case Invalid(e) => f(e)
    case Valid(a) => a
  }

  /**
   * Is this Valid and matching the given predicate
   */
  def exists(predicate: A => Boolean): Boolean = this match {
    case Valid(a) => predicate(a)
    case _ => false
  }

  /**
   * Is this Invalid or matching the predicate
   */
  def forall(f: A => Boolean): Boolean = this match {
    case Valid(a) => f(a)
    case _ => true
  }

  /**
   * Return this if it is Valid, or else fall back to the given default.
   * The functionality is similar to that of [[findValid]] except for failure accumulation,
   * where here only the error on the right is preserved and the error on the left is ignored.
   */
  def orElse[EE, AA >: A](default: => Validated[EE, AA]): Validated[EE, AA] =
    this match {
      case v @ Valid(_) => v
      case _ => default
    }

  /**
    * If `this` is valid return `this`, otherwise if `that` is valid return `that`, otherwise combine the failures.
    * This is similar to [[orElse]] except that here failures are accumulated.
    */
  def findValid[EE >: E, AA >: A](that: => Validated[EE, AA])(implicit EE: Semigroup[EE]): Validated[EE, AA] = this match {
    case v @ Valid(_) => v
    case Invalid(e) => that match {
      case v @ Valid(_) => v
      case Invalid(ee) => Invalid(EE.combine(e, ee))
    }
  }

  /**
   * Converts the value to an Either[E, A]
   */
  def toEither: Either[E, A] = this match {
    case Invalid(e) => Left(e)
    case Valid(a) => Right(a)
  }

  /**
   * Returns Valid values wrapped in Some, and None for Invalid values
   */
  def toOption: Option[A] = this match {
    case Valid(a) => Some(a)
    case _ => None
  }

  /**
    * Returns Valid values wrapped in Ior.Right, and None for Ior.Left values
    */
  def toIor: Ior[E, A] = this match {
    case Invalid(e) => Ior.Left(e)
    case Valid(a) => Ior.Right(a)
  }

  /**
   * Convert this value to a single element List if it is Valid,
   * otherwise return an empty List
   */
  def toList: List[A] = this match {
    case Valid(a) => List(a)
    case _ => Nil
  }

  /** Lift the Invalid value into a NonEmptyList. */
  def toValidatedNel[EE >: E, AA >: A]: ValidatedNel[EE, AA] =
    this match {
      case v @ Valid(_) => v
      case Invalid(e)  => Validated.invalidNel(e)
    }

  /**
   * Convert to an Either, apply a function, convert back.  This is handy
   * when you want to use the Monadic properties of the Either type.
   */
  def withEither[EE, B](f: Either[E, A] => Either[EE, B]): Validated[EE, B] =
    Validated.fromEither(f(toEither))

  /**
   * Validated is a [[Bifunctor]], this method applies one of the
   * given functions.
   */
  def bimap[EE, AA](fe: E => EE, fa: A => AA): Validated[EE, AA] =
    fold(fe andThen Invalid.apply,
         fa andThen Valid.apply)

  def compare[EE >: E, AA >: A](that: Validated[EE, AA])(implicit EE: Order[EE], AA: Order[AA]): Int = (this, that) match {
    case (Valid(a), Valid(aa)) => AA.compare(a, aa)
    case (Invalid(e), Invalid(ee)) => EE.compare(e, ee)
    case (Invalid(_), _) => -1
    case (Valid(_), _) => 1
  }

  def partialCompare[EE >: E, AA >: A](that: Validated[EE, AA])(implicit EE: PartialOrder[EE], AA: PartialOrder[AA]): Double = (this, that) match {
    case (Valid(a), Valid(aa)) => AA.partialCompare(a, aa)
    case (Invalid(e), Invalid(ee)) => EE.partialCompare(e, ee)
    case (Invalid(_), _) => -1
    case (Valid(_), _) => 1
  }

  def ===[EE >: E, AA >: A](that: Validated[EE, AA])(implicit EE: Eq[EE], AA: Eq[AA]): Boolean = (this, that) match {
    case (Invalid(e), Invalid(ee)) => EE.eqv(e, ee)
    case (Valid(a), Valid(aa)) => AA.eqv(a, aa)
    case _ => false
  }

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
  def map[B](f: A => B): Validated[E, B] = this match {
    case i @ Invalid(_) => i
    case Valid(a) => Valid(f(a))
  }

  /**
   * Apply a function to an Invalid value, returning a new Invalid value.
   * Or, if the original valid was Valid, return it.
   */
  def leftMap[EE](f: E => EE): Validated[EE, A] = this match {
    case a @ Valid(_) => a
    case Invalid(e) => Invalid(f(e))
  }

  /**
   * When Valid, apply the function, marking the result as valid
   * inside the Applicative's context,
   * when Invalid, lift the Error into the Applicative's context
   */
  def traverse[F[_], EE >: E, B](f: A => F[B])(implicit F: Applicative[F]): F[Validated[EE, B]] = this match {
    case Valid(a) => F.map(f(a))(Valid.apply)
    case e @ Invalid(_) => F.pure(e)
  }

  /**
   * apply the given function to the value with the given B when
   * valid, otherwise return the given B
   */
  def foldLeft[B](b: B)(f: (B, A) => B): B = this match {
    case Valid(a) => f(b, a)
    case _ => b
    }

  /**
   * Lazily-apply the given function to the value with the given B
   * when valid, otherwise return the given B.
   */
  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = this match {
    case Valid(a) => f(a, lb)
    case _ => lb
  }


  def show[EE >: E, AA >: A](implicit EE: Show[EE], AA: Show[AA]): String = this match {
    case Invalid(e) => s"Invalid(${EE.show(e)})"
    case Valid(a) => s"Valid(${AA.show(a)})"
  }


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

  def merge[EE >: E](implicit ev: A <:< EE): EE = this match {
    case Invalid(e) => e
    case Valid(a) => ev(a)
  }

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
  def ensure[EE >: E](onFailure: => EE)(f: A => Boolean): Validated[EE, A] = this match {
    case Valid(a) => if (f(a)) this else Validated.invalid(onFailure)
    case _ => this
  }

  /**
    * Ensure that a successful result passes the given predicate,
    * falling back to the an Invalid of the result of `onFailure` if the predicate
    * returns false.
    *
    * For example:
    * {{{
    * scala> Validated.valid("ab").ensureOr(s => new IllegalArgumentException("Must be longer than 3, provided '" + s + "'"))(_.length > 3)
    * res0: Validated[IllegalArgumentException, String] = Invalid(java.lang.IllegalArgumentException: Must be longer than 3, provided 'ab')
    * }}}
    */
  def ensureOr[EE >: E](onFailure: A => EE)(f: A => Boolean): Validated[EE, A] = this match {
    case Valid(a) => if (f(a)) this else Validated.invalid(onFailure(a))
    case _ => this
  }
}

object Validated extends ValidatedInstances with ValidatedFunctions{
  final case class Valid[+A](a: A) extends Validated[Nothing, A]
  final case class Invalid[+E](e: E) extends Validated[E, Nothing]


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

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class CatchOnlyPartiallyApplied[T](val dummy: Boolean = true ) extends AnyVal{
    def apply[A](f: => A)(implicit T: ClassTag[T], NT: NotNull[T]): Validated[T, A] =
      try {
        valid(f)
      } catch {
        case t if T.runtimeClass.isInstance(t) =>
          invalid(t.asInstanceOf[T])
      }
  }
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

  implicit def catsDataApplicativeErrorForValidated[E](implicit E: Semigroup[E]): ApplicativeError[Validated[E, ?], E] =
    new ValidatedApplicative[E] with ApplicativeError[Validated[E, ?], E] {

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

  implicit def catsDataCommutativeApplicativeForValidated[E: CommutativeSemigroup]: CommutativeApplicative[Validated[E, ?]] =
    new ValidatedApplicative[E] with CommutativeApplicative[Validated[E, ?]]

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

  // scalastyle:off method.length
  implicit def catsDataTraverseFunctorForValidated[E]: Traverse[Validated[E, ?]] =
    new Traverse[Validated[E, ?]] {

      override def traverse[G[_] : Applicative, A, B](fa: Validated[E, A])(f: (A) => G[B]): G[Validated[E, B]] =
        fa.traverse(f)

      override def foldLeft[A, B](fa: Validated[E, A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: Validated[E, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)

      override def map[A, B](fa: Validated[E, A])(f: (A) => B): Validated[E, B] =
        fa.map(f)

      override def reduceLeftToOption[A, B](fa: Validated[E, A])(f: A => B)(g: (B, A) => B): Option[B] =
        fa.map(f).toOption

      override def reduceRightToOption[A, B](fa: Validated[E, A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]] =
        Now(fa.map(f).toOption)

      override def reduceLeftOption[A](fa: Validated[E, A])(f: (A, A) => A): Option[A] =
        fa.toOption

      override def reduceRightOption[A](fa: Validated[E, A])(f: (A, Eval[A]) => Eval[A]): Eval[Option[A]] =
        Now(fa.toOption)

      override def size[A](fa: Validated[E, A]): Long = fa match {
        case Invalid(_) => 0L
        case _ => 1L
      }

      override def get[A](fa: Validated[E, A])(idx: Long): Option[A] =
        if (idx == 0L) fa.toOption else None

      override def foldMap[A, B](fa: Validated[E, A])(f: A => B)(implicit B: Monoid[B]): B = fa match {
        case Valid(a) => f(a)
        case _ => B.empty
      }

      override def find[A](fa: Validated[E, A])(f: A => Boolean): Option[A] =
        fa.toOption.filter(f)

      override def exists[A](fa: Validated[E, A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: Validated[E, A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def toList[A](fa: Validated[E, A]): List[A] = fa match {
        case Valid(a) => a :: Nil
        case _ => Nil
      }

      override def isEmpty[A](fa: Validated[E, A]): Boolean = fa.isInvalid
    }
  // scalastyle:off method.length
}

private[data] class ValidatedApplicative[E: Semigroup] extends CommutativeApplicative[Validated[E, ?]] {
  override def map[A, B](fa: Validated[E, A])(f: A => B): Validated[E, B] =
    fa.map(f)

  def pure[A](a: A): Validated[E, A] = Validated.valid(a)

  def ap[A, B](ff: Validated[E, (A) => B])(fa: Validated[E, A]): Validated[E, B] =
    fa.ap(ff)(Semigroup[E])

  override def product[A, B](fa: Validated[E, A], fb: Validated[E, B]): Validated[E, (A, B)] =
    fa.product(fb)(Semigroup[E])
}

private[data] trait ValidatedFunctions {
  /**
    * Converts an `A` to a `Validated[A, B]`.
    *
    * For example:
    * {{{
    * scala> Validated.invalid[IllegalArgumentException, String](new IllegalArgumentException("Argument is nonzero"))
    * res0: Validated[IllegalArgumentException, String] = Invalid(java.lang.IllegalArgumentException: Argument is nonzero)
    * }}}
    */
  def invalid[A, B](a: A): Validated[A, B] = Validated.Invalid(a)

  /**
    * Converts an `A` to a `ValidatedNel[A, B]`.
    *
    * For example:
    * {{{
    * scala> Validated.invalidNel[IllegalArgumentException, String](new IllegalArgumentException("Argument is nonzero"))
    * res0: ValidatedNel[IllegalArgumentException, String] = Invalid(NonEmptyList(java.lang.IllegalArgumentException: Argument is nonzero))
    * }}}
    */
  def invalidNel[A, B](a: A): ValidatedNel[A, B] = Validated.Invalid(NonEmptyList(a, Nil))

  /**
    * Converts a `B` to a `Validated[A, B]`.
    *
    * For example:
    * {{{
    * scala> Validated.valid[IllegalArgumentException, String]("Hello world")
    * res0: Validated[IllegalArgumentException, String] = Valid(Hello world)
    * }}}
    */
  def valid[A, B](b: B): Validated[A, B] = Validated.Valid(b)

  /**
    * Converts a `B` to a `ValidatedNel[A, B]`.
    *
    * For example:
    * {{{
    * scala> Validated.validNel[IllegalArgumentException, String]("Hello world")
    * res0: ValidatedNel[IllegalArgumentException, String] = Valid(Hello world)
    * }}}
    */
  def validNel[A, B](b: B): ValidatedNel[A, B] = Validated.Valid(b)

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
   * Converts an `Either[A, B]` to a `Validated[A, B]`.
   */
  def fromEither[A, B](e: Either[A, B]): Validated[A, B] = e.fold(invalid, valid)

  /**
   * Converts an `Option[B]` to a `Validated[A, B]`, where the provided `ifNone` values is returned on
   * the invalid of the `Validated` when the specified `Option` is `None`.
   */
  def fromOption[A, B](o: Option[B], ifNone: => A): Validated[A, B] = o.fold(invalid[A, B](ifNone))(valid)

  /**
    * Converts an `Ior[A, B]` to a `Validated[A, B]`.
    */
  def fromIor[A, B](ior: Ior[A, B]): Validated[A, B] = ior.fold(invalid, valid, (_, b) => valid(b))
}
