package cats
package data

import cats.syntax.either._
import cats.syntax.option._

final case class IorT[F[_], A, B](value: F[Ior[A, B]]) {

  def fold[C](fa: A => C, fb: B => C, fab: (A, B) => C)(implicit F: Functor[F]): F[C] = F.map(value)(_.fold(fa, fb, fab))

  def isLeft(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isLeft)

  def isRight(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isRight)

  def isBoth(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isBoth)

  def swap(implicit F: Functor[F]): IorT[F, B, A] = IorT(F.map(value)(_.swap))

  def getOrElse[BB >: B](default: => BB)(implicit F: Functor[F]): F[BB] = F.map(value)(_.getOrElse(default))

  def getOrElseF[BB >: B](default: => F[BB])(implicit F: Monad[F]): F[BB] =
    F.flatMap(value) {
      case Ior.Left(_) => default
      case Ior.Right(b) => F.pure(b)
      case Ior.Both(_, b) => F.pure(b)
    }

  def valueOr[BB >: B](f: A => BB)(implicit F: Functor[F], BB: Semigroup[BB]): F[BB] = F.map(value)(_.valueOr(f))

  def forall(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.forall(f))

  def exists(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.exists(f))

  def toOption(implicit F: Functor[F]): OptionT[F, B] = OptionT(F.map(value)(_.toOption))

  def toEither(implicit F: Functor[F]): EitherT[F, A, B] = EitherT(F.map(value)(_.toEither))

  def toNested: Nested[F, Ior[A, ?], B] = Nested[F, Ior[A, ?], B](value)

  def merge[AA >: A](implicit ev: B <:< AA, F: Functor[F], AA: Semigroup[AA]): F[AA] = F.map(value)(_.merge(ev, AA))

  def show(implicit show: Show[F[Ior[A, B]]]): String = show.show(value)
}

object IorT {

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class LeftPartiallyApplied[B](val dummy: Boolean = true) extends AnyVal {
    def apply[F[_], A](fa: F[A])(implicit F: Functor[F]): IorT[F, A, B] = IorT(F.map(fa)(Ior.left))
  }

  /**
   * Creates a left version of `IorT[F, A, B]` from a `F[A]`
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.implicits._
   * scala> IorT.left[Int](Option("err"))
   * res0: cats.data.IorT[Option,String,Int] = IorT(Some(Left(err)))
   * }}}
   */
  final def left[B]: LeftPartiallyApplied[B] = new LeftPartiallyApplied[B]

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class LeftTPartiallyApplied[F[_], B](val dummy: Boolean = true) extends AnyVal {
    def apply[A](a: A)(implicit F: Applicative[F]): IorT[F, A, B] = IorT(F.pure(Ior.left(a)))
  }

  /**
   * Creates a left version of `IorT[F, A, B]` from a `A`
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.implicits._
   * scala> IorT.leftT[Option, Int]("err")
   * res0: cats.data.IorT[Option,String,Int] = IorT(Some(Left(err)))
   * }}}
   */
  final def leftT[F[_], B]: LeftTPartiallyApplied[F, B] = new LeftTPartiallyApplied[F, B]

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class RightPartiallyApplied[A](val dummy: Boolean = true) extends AnyVal {
    def apply[F[_], B](fb: F[B])(implicit F: Functor[F]): IorT[F, A, B] = IorT(F.map(fb)(Ior.right))
  }

  /**
   * Creates a right version of `IorT[F, A, B]` from a `F[B]`
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.implicits._
   * scala> IorT.right[String](Option(3))
   * res0: cats.data.IorT[Option,String,Int] = IorT(Some(Right(3)))
   * }}}
   */
  final def right[A]: RightPartiallyApplied[A] = new RightPartiallyApplied[A]

  /**
   * Alias for [[pure]]
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.implicits._
   * scala> IorT.rightT[Option, String](3)
   * res0: cats.data.IorT[Option,String,Int] = IorT(Some(Right(3)))
   * }}}
   */
  final def rightT[F[_], A]: PurePartiallyApplied[F, A] = pure

  /**
   * Creates a both version of `IorT[F, A, B]` from a `F[A]` and a `F[B]`
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.implicits._
   * scala> IorT.both(Option("err"), Option(3))
   * res0: cats.data.IorT[Option,String,Int] = IorT(Some(Both(err,3)))
   * }}}
   */
  final def both[F[_], A, B](fa: F[A], fb: F[B])(implicit F: Apply[F]): IorT[F, A, B] =
    IorT(F.map2(fa, fb)((a, b) => Ior.Both(a, b)))

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class BothTPartiallyApplied[F[_]](val dummy: Boolean = true) extends AnyVal {
    def apply[A, B](a: A, b: B)(implicit F: Applicative[F]): IorT[F, A, B] = IorT(F.pure(Ior.Both(a, b)))
  }

  /**
   * Creates a both version of `IorT[F, A, B]` from a `A` and a `B`
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.implicits._
   * scala> IorT.bothT[Option]("err", 3)
   * res0: cats.data.IorT[Option,String,Int] = IorT(Some(Both(err,3)))
   * }}}
   */
  final def bothT[F[_]]: BothTPartiallyApplied[F] = new BothTPartiallyApplied[F]

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class PurePartiallyApplied[F[_], A](val dummy: Boolean = true) extends AnyVal {
    def apply[B](b: B)(implicit F: Applicative[F]): IorT[F, A, B] = IorT(F.pure(Ior.right(b)))
  }

  /**
   * Creates a right version of `IorT[F, A, B]` from a `B`
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.implicits._
   * scala> IorT.pure[Option, String](3)
   * res0: cats.data.IorT[Option,String,Int] = IorT(Some(Right(3)))
   * }}}
   */
  final def pure[F[_], A]: PurePartiallyApplied[F, A] = new PurePartiallyApplied[F, A]

  /**
   * Alias for [[right]]
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.implicits._
   * scala> val o: Option[Int] = Some(3)
   * scala> val n: Option[Int] = None
   * scala> IorT.liftF(o)
   * res0: cats.data.IorT[Option,Nothing,Int] = IorT(Some(Right(3)))
   * scala> IorT.liftF(n)
   * res1: cats.data.IorT[Option,Nothing,Int] = IorT(None)
   * }}}
   */
  final def liftF[F[_], A, B](fb: F[B])(implicit F: Applicative[F]): IorT[F, A, B] = right(fb)

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class FromIorPartiallyApplied[F[_]](val dummy: Boolean = true) extends AnyVal {
    def apply[A, B](ior: Ior[A, B])(implicit F: Applicative[F]): IorT[F, A, B] = IorT(F.pure(ior))
  }

  /**
   * Transforms an `Ior` into an `IorT`, lifted into the specified `Applicative`.
   * {{{
   * scala> import cats.data.{IorT, Ior}
   * scala> import cats.implicits._
   * scala> val i: Ior[String, Int] = Ior.both("warning", 3)
   * scala> IorT.fromIor[Option](i)
   * res0: cats.data.IorT[Option,String,Int] = IorT(Some(Both(warning,3)))
   * }}}
   */
  final def fromIor[F[_]]: FromIorPartiallyApplied[F] = new FromIorPartiallyApplied[F]

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class FromEitherPartiallyApplied[F[_]](val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](either: Either[E, A])(implicit F: Applicative[F]): IorT[F, E, A] = IorT(F.pure(either.toIor))
  }

  /**
   * Transforms an `Either` into an `IorT`, lifted into the specified `Applicative`.
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.implicits._
   * scala> val e: Either[String, Int] = Either.right(3)
   * scala> IorT.fromEither[Option](e)
   * res0: cats.data.IorT[Option,String,Int] = IorT(Some(Right(3)))
   * }}}
   */
  final def fromEither[F[_]]: FromEitherPartiallyApplied[F] = new FromEitherPartiallyApplied[F]

  /**
   * Transforms an `F[Either]` into an `IorT`.
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.implicits._
   * scala> val e: Either[String, Int] = Either.right(3)
   * scala> IorT.fromEitherF(Option(e))
   * res0: cats.data.IorT[Option,String,Int] = IorT(Some(Right(3)))
   * }}}
   */
  final def fromEitherF[F[_], E, A](feither: F[Either[E, A]])(implicit F: Functor[F]): IorT[F, E, A] =
    IorT(F.map(feither)(_.toIor))

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class FromOptionPartiallyApplied[F[_]](val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](option: Option[A], ifNone: => E)(implicit F: Applicative[F]): IorT[F, E, A] =
      IorT(F.pure(option.toRightIor(ifNone)))
  }

  /**
   * Transforms an `Option` into an `IorT`, lifted into the specified `Applicative` and using
   * the second argument if the `Option` is a `None`.
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.implicits._
   * scala> val o: Option[Int] = None
   * scala> IorT.fromOption[List](o, "Answer not known.")
   * res0: cats.data.IorT[List,String,Int] = IorT(List(Left(Answer not known.)))
   * scala> IorT.fromOption[List](Some(42), "Answer not known.")
   * res1: cats.data.IorT[List,String,Int] = IorT(List(Right(42)))
   * }}}
   */
  final def fromOption[F[_]]: FromOptionPartiallyApplied[F] = new FromOptionPartiallyApplied[F]

  /**
   * Transforms an `F[Option]` into an `IorT`, using the second argument if the `Option` is a `None`.
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.implicits._
   * scala> val o: Option[Int] = None
   * scala> IorT.fromOptionF(List(o), "Answer not known.")
   * res0: cats.data.IorT[List,String,Int]  = IorT(List(Left(Answer not known.)))
   * scala> IorT.fromOptionF(List(Option(42)), "Answer not known.")
   * res1: cats.data.IorT[List,String,Int] = IorT(List(Right(42)))
   * }}}
   */
  final def fromOptionF[F[_], E, A](foption: F[Option[A]], ifNone: => E)(implicit F: Functor[F]): IorT[F, E, A] =
    IorT(F.map(foption)(_.toRightIor(ifNone)))

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class CondPartiallyApplied[F[_]](val dummy: Boolean = true) extends AnyVal {
    def apply[A, B](test: Boolean, right: => B, left: => A)(implicit F: Applicative[F]): IorT[F, A, B] =
      IorT(F.pure(if (test) Ior.right(right) else Ior.left(left)))
  }

  /**
   * If the condition is satisfied, return the given `B` in `Right`, otherwise, return the given
   * `A` in `Left`, lifted into the specified `Applicative`.
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.implicits._
   * scala> val userInput = "hello world"
   * scala> IorT.cond[Option](
   *      |   userInput.forall(_.isDigit) && userInput.size == 10,
   *      |   userInput,
   *      |   "The input does not look like a phone number")
   * res0: IorT[Option, String, String] = IorT(Some(Left(The input does not look like a phone number)))
   * }}}
   */
  final def cond[F[_]]: CondPartiallyApplied[F] = new CondPartiallyApplied[F]
}
