package cats
package data

import cats.Bifunctor
import cats.instances.either._
import cats.syntax.either._

/**
 * Transformer for `Either`, allowing the effect of an arbitrary type constructor `F` to be combined with the
 * fail-fast effect of `Either`.
 *
 * `EitherT[F, A, B]` wraps a value of type `F[Either[A, B]]`. An `F[C]` can be lifted in to `EitherT[F, A, C]` via `EitherT.right`,
 * and lifted in to a `EitherT[F, C, B]` via `EitherT.left`.
 */
final case class EitherT[F[_], A, B](value: F[Either[A, B]]) {
  def fold[C](fa: A => C, fb: B => C)(implicit F: Functor[F]): F[C] = F.map(value)(_.fold(fa, fb))

  def isLeft(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isLeft)

  def isRight(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isRight)

  def swap(implicit F: Functor[F]): EitherT[F, B, A] = EitherT(F.map(value)(_.swap))

  def getOrElse[BB >: B](default: => BB)(implicit F: Functor[F]): F[BB] = F.map(value)(_.getOrElse(default))

  def getOrElseF[BB >: B](default: => F[BB])(implicit F: Monad[F]): F[BB] = {
    F.flatMap(value) {
      case Left(_) => default
      case Right(b) => F.pure(b)
    }
  }

  def orElse[AA, BB >: B](default: => EitherT[F, AA, BB])(implicit F: Monad[F]): EitherT[F, AA, BB] = {
    EitherT(F.flatMap(value) {
      case Left(_) => default.value
      case r @ Right(_) => F.pure(r.leftCast)
    })
  }

  def recover(pf: PartialFunction[A, B])(implicit F: Functor[F]): EitherT[F, A, B] =
    EitherT(F.map(value)(_.recover(pf)))

  def recoverWith(pf: PartialFunction[A, EitherT[F, A, B]])(implicit F: Monad[F]): EitherT[F, A, B] =
    EitherT(F.flatMap(value) {
      case Left(a) if pf.isDefinedAt(a) => pf(a).value
      case other => F.pure(other)
    })

  def valueOr[BB >: B](f: A => BB)(implicit F: Functor[F]): F[BB] = fold(f, identity)

  def valueOrF[BB >: B](f: A => F[BB])(implicit F: Monad[F]): F[BB] = {
      F.flatMap(value){
        case Left(a) => f(a)
        case Right(b) => F.pure(b)
      }
  }

  def forall(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.forall(f))

  def exists(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.exists(f))

  def ensure[AA >: A](onFailure: => AA)(f: B => Boolean)(implicit F: Functor[F]): EitherT[F, AA, B] =
    EitherT(F.map(value)(_.ensure(onFailure)(f)))

  def ensureOr[AA >: A](onFailure: B => AA)(f: B => Boolean)(implicit F: Functor[F]): EitherT[F, AA, B] =
    EitherT(F.map(value)(_.ensureOr(onFailure)(f)))

  def toOption(implicit F: Functor[F]): OptionT[F, B] = OptionT(F.map(value)(_.toOption))

  def to[G[_]](implicit F: Functor[F], G: Alternative[G]): F[G[B]] =
    F.map(value)(_.to[G])

  def collectRight(implicit FA: Alternative[F], FM: Monad[F]): F[B] =
    FM.flatMap(value)(_.to[F])

  def bimap[C, D](fa: A => C, fb: B => D)(implicit F: Functor[F]): EitherT[F, C, D] = EitherT(F.map(value)(_.bimap(fa, fb)))

  def bitraverse[G[_], C, D](f: A => G[C], g: B => G[D])(implicit traverseF: Traverse[F], applicativeG: Applicative[G]): G[EitherT[F, C, D]] =
    applicativeG.map(traverseF.traverse(value)(axb => Bitraverse[Either].bitraverse(axb)(f, g)))(EitherT.apply)

  def applyAlt[D](ff: EitherT[F, A, B => D])(implicit F: Apply[F]): EitherT[F, A, D] =
    EitherT[F, A, D](F.map2(this.value, ff.value)((xb, xbd) => Apply[Either[A, ?]].ap(xbd)(xb)))

  def flatMap[AA >: A, D](f: B => EitherT[F, AA, D])(implicit F: Monad[F]): EitherT[F, AA, D] =
    EitherT(F.flatMap(value) {
      case l @ Left(_) => F.pure(l.rightCast)
      case Right(b) => f(b).value
    })

  def flatMapF[AA >: A, D](f: B => F[Either[AA, D]])(implicit F: Monad[F]): EitherT[F, AA, D] =
    flatMap(f andThen EitherT.apply)

  def transform[C, D](f: Either[A, B] => Either[C, D])(implicit F: Functor[F]): EitherT[F, C, D] =
    EitherT(F.map(value)(f))

  def subflatMap[AA >: A, D](f: B => Either[AA, D])(implicit F: Functor[F]): EitherT[F, AA, D] =
    transform(_.flatMap(f))

  def map[D](f: B => D)(implicit F: Functor[F]): EitherT[F, A, D] = bimap(identity, f)

  /**
   * Modify the context `F` using transformation `f`.
   */
  def mapK[G[_]](f: F ~> G): EitherT[G, A, B] = EitherT[G, A, B](f(value))

  def semiflatMap[D](f: B => F[D])(implicit F: Monad[F]): EitherT[F, A, D] =
    flatMap(b => EitherT.right(f(b)))

  def leftMap[C](f: A => C)(implicit F: Functor[F]): EitherT[F, C, B] = bimap(f, identity)

  def leftFlatMap[BB >: B, D](f: A => EitherT[F, D, BB])(implicit F: Monad[F]): EitherT[F, D, BB] =
    EitherT(F.flatMap(value) {
      case Left(a) => f(a).value
      case r@Right(_) => F.pure(r.leftCast)
    })

  def leftSemiflatMap[D](f: A => F[D])(implicit F: Monad[F]): EitherT[F, D, B] =
    EitherT(F.flatMap(value) {
      case Left(a) => F.map(f(a)) { d => Left(d) }
      case r@Right(_) => F.pure(r.leftCast)
    })

  def compare(that: EitherT[F, A, B])(implicit o: Order[F[Either[A, B]]]): Int =
    o.compare(value, that.value)

  def partialCompare(that: EitherT[F, A, B])(implicit p: PartialOrder[F[Either[A, B]]]): Double =
    p.partialCompare(value, that.value)

  def ===(that: EitherT[F, A, B])(implicit eq: Eq[F[Either[A, B]]]): Boolean =
    eq.eqv(value, that.value)

  def traverse[G[_], D](f: B => G[D])(implicit traverseF: Traverse[F], applicativeG: Applicative[G]): G[EitherT[F, A, D]] =
    applicativeG.map(traverseF.traverse(value)(axb => Traverse[Either[A, ?]].traverse(axb)(f)))(EitherT.apply)

  def foldLeft[C](c: C)(f: (C, B) => C)(implicit F: Foldable[F]): C =
    F.foldLeft(value, c)((c, axb) => axb.foldLeft(c)(f))

  def foldRight[C](lc: Eval[C])(f: (B, Eval[C]) => Eval[C])(implicit F: Foldable[F]): Eval[C] =
    F.foldRight(value, lc)((axb, lc) => axb.foldRight(lc)(f))

  def merge[AA >: A](implicit ev: B <:< AA, F: Functor[F]): F[AA] = F.map(value)(_.fold(identity, ev.apply))

  /**
   * Similar to `Either#combine` but mapped over an `F` context.
   *
   * Examples:
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> val l1: EitherT[Option, String, Int] = EitherT.left(Some("error 1"))
   * scala> val l2: EitherT[Option, String, Int] = EitherT.left(Some("error 2"))
   * scala> val r3: EitherT[Option, String, Int] = EitherT.right(Some(3))
   * scala> val r4: EitherT[Option, String, Int] = EitherT.right(Some(4))
   * scala> val noneEitherT: EitherT[Option, String, Int] = EitherT.left(None)
   *
   * scala> l1 combine l2
   * res0: EitherT[Option, String, Int] = EitherT(Some(Left(error 1)))
   *
   * scala> l1 combine r3
   * res1: EitherT[Option, String, Int] = EitherT(Some(Left(error 1)))
   *
   * scala> r3 combine l1
   * res2: EitherT[Option, String, Int] = EitherT(Some(Left(error 1)))
   *
   * scala> r3 combine r4
   * res3: EitherT[Option, String, Int] = EitherT(Some(Right(7)))
   *
   * scala> l1 combine noneEitherT
   * res4: EitherT[Option, String, Int] = EitherT(None)
   *
   * scala> noneEitherT combine l1
   * res5: EitherT[Option, String, Int] = EitherT(None)
   *
   * scala> r3 combine noneEitherT
   * res6: EitherT[Option, String, Int] = EitherT(None)
   *
   * scala> noneEitherT combine r4
   * res7: EitherT[Option, String, Int] = EitherT(None)
   * }}}
   */
  def combine(that: EitherT[F, A, B])(implicit F: Apply[F], B: Semigroup[B]): EitherT[F, A, B] =
    EitherT(F.map2(this.value, that.value)(_ combine _))

  def toValidated(implicit F: Functor[F]): F[Validated[A, B]] =
    F.map(value)(_.toValidated)

  def toValidatedNel(implicit F: Functor[F]): F[ValidatedNel[A, B]] =
    F.map(value)(_.toValidatedNel)

  /** Run this value as a `[[Validated]]` against the function and convert it back to an `[[EitherT]]`.
   *
   * The [[Applicative]] instance for `EitherT` "fails fast" - it is often useful to "momentarily" have
   * it accumulate errors instead, which is what the `[[Validated]]` data type gives us.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> type Error = String
   * scala> val v1: Validated[NonEmptyList[Error], Int] = Validated.invalidNel("error 1")
   * scala> val v2: Validated[NonEmptyList[Error], Int] = Validated.invalidNel("error 2")
   * scala> val eithert: EitherT[Option, Error, Int] = EitherT.leftT[Option, Int]("error 3")
   * scala> eithert.withValidated { v3 => (v1, v2, v3.toValidatedNel).mapN { case (i, j, k) => i + j + k } }
   * res0: EitherT[Option, NonEmptyList[Error], Int] = EitherT(Some(Left(NonEmptyList(error 1, error 2, error 3))))
   * }}}
   */
  def withValidated[AA, BB](f: Validated[A, B] => Validated[AA, BB])(implicit F: Functor[F]): EitherT[F, AA, BB] =
    EitherT(F.map(value)(either => f(either.toValidated).toEither))

  def show(implicit show: Show[F[Either[A, B]]]): String = show.show(value)

  /**
   * Transform this `EitherT[F, A, B]` into a `[[Nested]][F, Either[A, ?], B]`.
   *
   * An example where `toNested` can be used, is to get the `Apply.ap` function with the
   * behavior from the composed `Apply` instances from `F` and `Either[A, ?]`, which is
   * inconsistent with the behavior of the `ap` from `Monad` of `EitherT`.
   *
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> val ff: EitherT[List, String, Int => String] =
   *      |   EitherT(List(Either.right(_.toString), Either.left("error")))
   * scala> val fa: EitherT[List, String, Int] =
   *      |   EitherT(List(Either.right(1), Either.right(2)))
   * scala> ff.ap(fa)
   * res0: EitherT[List,String,String] = EitherT(List(Right(1), Right(2), Left(error)))
   * scala> EitherT((ff.toNested).ap(fa.toNested).value)
   * res1: EitherT[List,String,String] = EitherT(List(Right(1), Right(2), Left(error), Left(error)))
   * }}}
   *
   */
  def toNested: Nested[F, Either[A, ?], B] = Nested[F, Either[A, ?], B](value)

  /**
    * Transform this `EitherT[F, A, B]` into a `[[Nested]][F, Validated[A, ?], B]`.
    *
    * Example:
    * {{{
    * scala> import cats.data.{EitherT, Validated}
    * scala> import cats.implicits._
    * scala> val f: Int => String = i => (i*2).toString
    * scala> val r1: EitherT[Option, String, Int => String] = EitherT.right(Some(f))
    * r1: cats.data.EitherT[Option,String,Int => String] = EitherT(Some(Right(<function1>)))
    * scala> val r2: EitherT[Option, String, Int] = EitherT.right(Some(10))
    * r2: cats.data.EitherT[Option,String,Int] = EitherT(Some(Right(10)))
    * scala> type ErrorOr[A] = Validated[String, A]
    * scala> (r1.toNestedValidated).ap(r2.toNestedValidated)
    * res0: cats.data.Nested[Option,ErrorOr,String] = Nested(Some(Valid(20)))
    * }}}
    */
  def toNestedValidated(implicit F: Functor[F]): Nested[F, Validated[A, ?], B] =
    Nested[F, Validated[A, ?], B](F.map(value)(_.toValidated))

  /**
   * Transform this `EitherT[F, A, B]` into a `[[Nested]][F, ValidatedNel[A, ?], B]`.
   */
  def toNestedValidatedNel(implicit F: Functor[F]): Nested[F, ValidatedNel[A, ?], B] =
    Nested[F, ValidatedNel[A, ?], B](F.map(value)(_.toValidatedNel))
}

object EitherT extends EitherTInstances {

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class LeftPartiallyApplied[B](val dummy: Boolean = true) extends AnyVal {
    def apply[F[_], A](fa: F[A])(implicit F: Functor[F]): EitherT[F, A, B] = EitherT(F.map(fa)(Either.left))
  }

  /**
   * Creates a left version of `EitherT[F, A, B]` from a `F[A]`
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> EitherT.left[Int](Option("err"))
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Left(err)))
   * }}}
   */
  final def left[B]: LeftPartiallyApplied[B] = new LeftPartiallyApplied[B]

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class LeftTPartiallyApplied[F[_], B](val dummy: Boolean = true) extends AnyVal {
    def apply[A](a: A)(implicit F: Applicative[F]): EitherT[F, A, B] = EitherT(F.pure(Either.left(a)))
  }

  /**
   * Creates a left version of `EitherT[F, A, B]` from a `A`
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> EitherT.leftT[Option, Int]("err")
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Left(err)))
   * }}}
   */
  final def leftT[F[_], B]: LeftTPartiallyApplied[F, B] = new LeftTPartiallyApplied[F, B]

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class RightPartiallyApplied[A](val dummy: Boolean = true) extends AnyVal {
    def apply[F[_], B](fb: F[B])(implicit F: Functor[F]): EitherT[F, A, B] = EitherT(F.map(fb)(Either.right))
  }

  /**
   * Creates a right version of `EitherT[F, A, B]` from a `F[B]`
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> EitherT.right[String](Option(3))
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Right(3)))
   * }}}
   */
  final def right[A]: RightPartiallyApplied[A] = new RightPartiallyApplied[A]

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class PurePartiallyApplied[F[_], A](val dummy: Boolean = true) extends AnyVal {
    def apply[B](b: B)(implicit F: Applicative[F]): EitherT[F, A, B] = right(F.pure(b))
  }

  /**
   * Creates a new `EitherT[F, A, B]` from a `B`
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> EitherT.pure[Option, String](3)
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Right(3)))
   * }}}
   */
  final def pure[F[_], A]: PurePartiallyApplied[F, A] = new PurePartiallyApplied[F, A]

  /**
   * Alias for [[pure]]
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> EitherT.rightT[Option, String](3)
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Right(3)))
   * }}}
   */
  final def rightT[F[_], A]: PurePartiallyApplied[F, A] = pure


  /**
   * Alias for [[right]]
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.implicits._
   * scala> val o: Option[Int] = Some(3)
   * scala> val n: Option[Int] = None
   * scala> EitherT.liftF(o)
   * res0: cats.data.EitherT[Option,Nothing,Int] = EitherT(Some(Right(3)))
   * scala> EitherT.liftF(n)
   * res1: cats.data.EitherT[Option,Nothing,Int] = EitherT(None)
   * }}}
   */
  final def liftF[F[_], A, B](fb: F[B])(implicit F: Functor[F]): EitherT[F, A, B] = right(fb)

  /**
   * Same as [[liftF]], but expressed as a FunctionK for use with mapK
   * {{{
   * scala> import cats._, data._, implicits._
   * scala> val a: OptionT[Eval, Int] = 1.pure[OptionT[Eval, ?]]
   * scala> val b: OptionT[EitherT[Eval, String, ?], Int] = a.mapK(EitherT.liftK)
   * scala> b.value.value.value
   * res0: Either[String,Option[Int]] = Right(Some(1))
   * }}}
   */
  final def liftK[F[_], A](implicit F: Functor[F]): F ~> EitherT[F, A, ?] =
    λ[F ~> EitherT[F, A, ?]](right(_))

  @deprecated("Use EitherT.liftF.", "1.0.0-RC1")
  final def liftT[F[_], A, B](fb: F[B])(implicit F: Functor[F]): EitherT[F, A, B] = right(fb)

  /** Transforms an `Either` into an `EitherT`, lifted into the specified `Applicative`.
   *
   * Note: The return type is a FromEitherPartiallyApplied[F], which has an apply method
   * on it, allowing you to call fromEither like this:
   * {{{
   * scala> import cats.implicits._
   * scala> val t: Either[String, Int] = Either.right(3)
   * scala> EitherT.fromEither[Option](t)
   * res0: EitherT[Option, String, Int] = EitherT(Some(Right(3)))
   * }}}
   *
   * The reason for the indirection is to emulate currying type parameters.
   */
  final def fromEither[F[_]]: FromEitherPartiallyApplied[F] = new FromEitherPartiallyApplied

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class FromEitherPartiallyApplied[F[_]](val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](either: Either[E, A])(implicit F: Applicative[F]): EitherT[F, E, A] =
      EitherT(F.pure(either))
  }

  /** Transforms an `Option` into an `EitherT`, lifted into the specified `Applicative` and using
   *  the second argument if the `Option` is a `None`.
   * {{{
   * scala> import cats.implicits._
   * scala> val o: Option[Int] = None
   * scala> EitherT.fromOption[List](o, "Answer not known.")
   * res0: EitherT[List, String, Int]  = EitherT(List(Left(Answer not known.)))
   * scala> EitherT.fromOption[List](Some(42), "Answer not known.")
   * res1: EitherT[List, String, Int] = EitherT(List(Right(42)))
   * }}}
   */
  final def fromOption[F[_]]: FromOptionPartiallyApplied[F] = new FromOptionPartiallyApplied

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class FromOptionPartiallyApplied[F[_]](val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](opt: Option[A], ifNone: => E)(implicit F: Applicative[F]): EitherT[F, E, A] =
      EitherT(F.pure(Either.fromOption(opt, ifNone)))
  }

  /** Transforms an `F[Option]` into an `EitherT`, using the second argument if the `Option` is a `None`.
   * {{{
   * scala> import cats.implicits._
   * scala> val o: Option[Int] = None
   * scala> EitherT.fromOptionF(List(o), "Answer not known.")
   * res0: EitherT[List, String, Int]  = EitherT(List(Left(Answer not known.)))
   * scala> EitherT.fromOptionF(List(Option(42)), "Answer not known.")
   * res1: EitherT[List, String, Int] = EitherT(List(Right(42)))
   * }}}
   */
  final def fromOptionF[F[_], E, A](fopt: F[Option[A]], ifNone: => E)(implicit F: Functor[F]): EitherT[F, E, A] =
    EitherT(F.map(fopt)(opt => Either.fromOption(opt, ifNone)))

  /**  If the condition is satisfied, return the given `A` in `Right`
    *  lifted into the specified `Applicative`, otherwise, return the
    *  given `E` in `Left` lifted into the specified `Applicative`.
    *
    * {{{
    * scala> import cats.Id
    * scala> import cats.data.EitherT
    * scala> val userInput = "hello world"
    * scala> EitherT.cond[Id](
    *      |   userInput.forall(_.isDigit) && userInput.size == 10,
    *      |   userInput,
    *      |   "The input does not look like a phone number")
    * res0: EitherT[Id, String, String] = EitherT(Left(The input does not look like a phone number))
    * }}}
    */
  final def cond[F[_]]: CondPartiallyApplied[F] = new CondPartiallyApplied

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  private[data] final class CondPartiallyApplied[F[_]](val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](test: Boolean, right: => A, left: => E)(implicit F: Applicative[F]): EitherT[F, E, A] =
      EitherT(F.pure(Either.cond(test, right, left)))
  }
}

private[data] abstract class EitherTInstances extends EitherTInstances1 {

  implicit def catsDataOrderForEitherT[F[_], L, R](implicit F: Order[F[Either[L, R]]]): Order[EitherT[F, L, R]] =
    new EitherTOrder[F, L, R] {
      val F0: Order[F[Either[L, R]]] = F
    }

  implicit def catsErrorControlForEitherT[F[_]: Monad, E]: ErrorControl[EitherT[F, E, ?], F, E] =
    new ErrorControl[EitherT[F, E, ?], F, E] {
      val monadErrorF: MonadError[EitherT[F, E, ?], E] = EitherT.catsDataMonadErrorForEitherT
      val monadG: Monad[F] = Monad[F]

      def controlError[A](fa: EitherT[F, E, A])(f: E => F[A]): F[A] =
        Monad[F].flatMap(fa.value) {
          case Left(e) => f(e)
          case Right(a) => monadG.pure(a)
        }

      def accept[A](ga: F[A]): EitherT[F, E, A] =
        EitherT.liftF(ga)

    }

  implicit def catsDataShowForEitherT[F[_], L, R](implicit sh: Show[F[Either[L, R]]]): Show[EitherT[F, L, R]] =
    Contravariant[Show].contramap(sh)(_.value)

  implicit def catsDataBifunctorForEitherT[F[_]](implicit F: Functor[F]): Bifunctor[EitherT[F, ?, ?]] =
    new Bifunctor[EitherT[F, ?, ?]] {
      override def bimap[A, B, C, D](fab: EitherT[F, A, B])(f: A => C, g: B => D): EitherT[F, C, D] = fab.bimap(f, g)
    }

  implicit def catsDataTraverseForEitherT[F[_], L](implicit F: Traverse[F]): Traverse[EitherT[F, L, ?]] =
    new EitherTTraverse[F, L] {
      val F0: Traverse[F] = F
    }

  implicit def catsMonoidForEitherT[F[_], L, A](implicit F: Monoid[F[Either[L, A]]]): Monoid[EitherT[F, L, A]] =
    new EitherTMonoid[F, L, A] { implicit val F0 = F }

}

private[data] abstract class EitherTInstances1 extends EitherTInstances2 {

  implicit def catsSemigroupForEitherT[F[_], L, A](implicit F: Semigroup[F[Either[L, A]]]): Semigroup[EitherT[F, L, A]] =
    new EitherTSemigroup[F, L, A] { implicit val F0 = F }

  implicit def catsDataFoldableForEitherT[F[_], L](implicit F: Foldable[F]): Foldable[EitherT[F, L, ?]] =
    new EitherTFoldable[F, L] {
      val F0: Foldable[F] = F
    }

  implicit def catsDataPartialOrderForEitherT[F[_], L, R](implicit F: PartialOrder[F[Either[L, R]]]): PartialOrder[EitherT[F, L, R]] =
    new EitherTPartialOrder[F, L, R] {
      val F0: PartialOrder[F[Either[L, R]]] = F
    }

  implicit def catsDataBitraverseForEitherT[F[_]](implicit F: Traverse[F]): Bitraverse[EitherT[F, ?, ?]] =
    new EitherTBitraverse[F] {
      val F0: Traverse[F] = F
    }

  implicit def catsDataMonadErrorForEitherT[F[_], L](implicit F0: Monad[F]): MonadError[EitherT[F, L, ?], L] =
    new EitherTMonadError[F, L] {
      implicit val F = F0
      override def ensure[A](fa: EitherT[F, L, A])(error: => L)(predicate: (A) => Boolean): EitherT[F, L, A] =
        fa.ensure(error)(predicate)(F)

      override def ensureOr[A](fa: EitherT[F, L, A])(error: (A) => L)(predicate: (A) => Boolean): EitherT[F, L, A] =
        fa.ensureOr(error)(predicate)(F)
    }
}

private[data] abstract class EitherTInstances2 extends EitherTInstances3 {
  /**  Monad error instance for recovering errors in F instead of
   *  the underlying Either.
   *
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.MonadError
   * scala> import cats.instances.option._
   * scala> val noInt: Option[Either[String, Int]] = None
   * scala> val et = EitherT[Option, String, Int](noInt)
   * scala> val me = MonadError[EitherT[Option, String, ?], Unit]
   * scala> me.recover(et) { case () => 1 }
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Right(1)))
   * }}}
   */
  implicit def catsDataMonadErrorFForEitherT[F[_], E, L](implicit FE0: MonadError[F, E]): MonadError[EitherT[F, L, ?], E] =
    new EitherTMonadErrorF[F, E, L] { implicit val F = FE0 }


  implicit def catsDataSemigroupKForEitherT[F[_], L](implicit F0: Monad[F]): SemigroupK[EitherT[F, L, ?]] =
    new EitherTSemigroupK[F, L] { implicit val F = F0 }

  implicit def catsDataEqForEitherT[F[_], L, R](implicit F: Eq[F[Either[L, R]]]): Eq[EitherT[F, L, R]] =
    new EitherTEq[F, L, R] {
      val F0: Eq[F[Either[L, R]]] = F
    }
}

private[data] abstract class EitherTInstances3 {
  implicit def catsDataFunctorForEitherT[F[_], L](implicit F0: Functor[F]): Functor[EitherT[F, L, ?]] =
    new EitherTFunctor[F, L] { implicit val F = F0 }
}

private[data] trait EitherTSemigroup[F[_], L, A] extends Semigroup[EitherT[F, L, A]] {
  implicit val F0: Semigroup[F[Either[L, A]]]
  def combine(x: EitherT[F, L , A], y: EitherT[F, L , A]): EitherT[F, L , A] =
    EitherT(F0.combine(x.value, y.value))
}

private[data] trait EitherTMonoid[F[_], L, A] extends Monoid[EitherT[F, L, A]] with EitherTSemigroup[F, L, A] {
  implicit val F0: Monoid[F[Either[L, A]]]
  def empty: EitherT[F, L, A] = EitherT(F0.empty)
}

private[data] trait EitherTSemigroupK[F[_], L] extends SemigroupK[EitherT[F, L, ?]] {
  implicit val F: Monad[F]
  def combineK[A](x: EitherT[F, L, A], y: EitherT[F, L, A]): EitherT[F, L, A] =
    EitherT(F.flatMap(x.value) {
      case l @ Left(_) => y.value
      case r @ Right(_) => F.pure(r)
    })
}

private[data] trait EitherTFunctor[F[_], L] extends Functor[EitherT[F, L, ?]] {
  implicit val F: Functor[F]
  override def map[A, B](fa: EitherT[F, L, A])(f: A => B): EitherT[F, L, B] = fa map f
}

private[data] trait EitherTMonad[F[_], L] extends Monad[EitherT[F, L, ?]] with EitherTFunctor[F, L] {
  implicit val F: Monad[F]
  def pure[A](a: A): EitherT[F, L, A] = EitherT.pure(a)

  def flatMap[A, B](fa: EitherT[F, L, A])(f: A => EitherT[F, L, B]): EitherT[F, L, B] = fa flatMap f
  def tailRecM[A, B](a: A)(f: A => EitherT[F, L, Either[A, B]]): EitherT[F, L, B] =
    EitherT(F.tailRecM(a)(a0 => F.map(f(a0).value) {
      case Left(l)         => Right(Left(l))
      case Right(Left(a1)) => Left(a1)
      case Right(Right(b)) => Right(Right(b))
    }))
}

private[data] trait EitherTMonadErrorF[F[_], E, L] extends MonadError[EitherT[F, L, ?], E] with EitherTMonad[F, L] {
  implicit val F: MonadError[F, E]

  def handleErrorWith[A](fea: EitherT[F, L, A])(f: E => EitherT[F, L, A]): EitherT[F, L, A] =
    EitherT(F.handleErrorWith(fea.value)(f(_).value))

  def raiseError[A](e: E): EitherT[F, L, A] = EitherT(F.raiseError(e))
}

private[data] trait EitherTMonadError[F[_], L] extends MonadError[EitherT[F, L, ?], L] with EitherTMonad[F, L] {
  def handleErrorWith[A](fea: EitherT[F, L, A])(f: L => EitherT[F, L, A]): EitherT[F, L, A] =
    EitherT(F.flatMap(fea.value) {
      case Left(e) => f(e).value
      case r @ Right(_) => F.pure(r)
    })
  override def handleError[A](fea: EitherT[F, L, A])(f: L => A): EitherT[F, L, A] =
    EitherT(F.flatMap(fea.value) {
      case Left(e) => F.pure(Right(f(e)))
      case r @ Right(_) => F.pure(r)
    })
  def raiseError[A](e: L): EitherT[F, L, A] = EitherT.left(F.pure(e))
  override def attempt[A](fla: EitherT[F, L, A]): EitherT[F, L, Either[L, A]] = EitherT.right(fla.value)
  override def recover[A](fla: EitherT[F, L, A])(pf: PartialFunction[L, A]): EitherT[F, L, A] =
    fla.recover(pf)
  override def recoverWith[A](fla: EitherT[F, L, A])(pf: PartialFunction[L, EitherT[F, L, A]]): EitherT[F, L, A] =
    fla.recoverWith(pf)
}

private[data] sealed trait EitherTFoldable[F[_], L] extends Foldable[EitherT[F, L, ?]] {
  implicit def F0: Foldable[F]

  def foldLeft[A, B](fa: EitherT[F, L, A], b: B)(f: (B, A) => B): B =
    fa.foldLeft(b)(f)

  def foldRight[A, B](fa: EitherT[F, L, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    fa.foldRight(lb)(f)
}

private[data] sealed trait EitherTTraverse[F[_], L] extends Traverse[EitherT[F, L, ?]] with EitherTFoldable[F, L] {
  override implicit def F0: Traverse[F]

  override def traverse[G[_]: Applicative, A, B](fa: EitherT[F, L, A])(f: A => G[B]): G[EitherT[F, L, B]] =
    fa traverse f
}

private[data] sealed trait EitherTBifoldable[F[_]] extends Bifoldable[EitherT[F, ?, ?]] {
  implicit def F0: Foldable[F]

  def bifoldLeft[A, B, C](fab: EitherT[F, A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    F0.foldLeft(fab.value, c)( (acc, axb) => Bifoldable[Either].bifoldLeft(axb, acc)(f, g))

  def bifoldRight[A, B, C](fab: EitherT[F, A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
    F0.foldRight(fab.value, c)( (axb, acc) => Bifoldable[Either].bifoldRight(axb, acc)(f, g))
}

private[data] sealed trait EitherTBitraverse[F[_]] extends Bitraverse[EitherT[F, ?, ?]] with EitherTBifoldable[F] {
  override implicit def F0: Traverse[F]

  override def bitraverse[G[_], A, B, C, D](fab: EitherT[F, A, B])(f: A => G[C], g: B => G[D])(implicit G: Applicative[G]): G[EitherT[F, C, D]] =
    fab.bitraverse(f, g)
}

private[data] sealed trait EitherTEq[F[_], L, A] extends Eq[EitherT[F, L, A]] {
  implicit def F0: Eq[F[Either[L, A]]]

  override def eqv(x: EitherT[F, L, A], y: EitherT[F, L, A]): Boolean = x === y
}

private[data] sealed trait EitherTPartialOrder[F[_], L, A] extends PartialOrder[EitherT[F, L, A]] with EitherTEq[F, L, A]{
  override implicit def F0: PartialOrder[F[Either[L, A]]]

  override def partialCompare(x: EitherT[F, L, A], y: EitherT[F, L, A]): Double =
    x partialCompare y
}

private[data] sealed trait EitherTOrder[F[_], L, A] extends Order[EitherT[F, L, A]] with EitherTPartialOrder[F, L, A]{
  override implicit def F0: Order[F[Either[L, A]]]

  override def compare(x: EitherT[F, L, A], y: EitherT[F, L, A]): Int = x compare y
}
