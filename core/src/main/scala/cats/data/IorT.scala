package cats
package data

import cats.arrow.FunctionK
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

  def toNestedValidated(implicit F: Functor[F]): Nested[F, Validated[A, ?], B] =
    Nested[F, Validated[A, ?], B](F.map(value)(_.toValidated))

  def toValidated(implicit F: Functor[F]): F[Validated[A, B]] = F.map(value)(_.toValidated)

  def to[G[_]](implicit F: Functor[F], G: Alternative[G]): F[G[B]] = F.map(value)(_.to[G, B])

  def collectRight(implicit FA: Alternative[F], FM: FlatMap[F]): F[B] = FM.flatMap(value)(_.to[F, B])

  def merge[AA >: A](implicit ev: B <:< AA, F: Functor[F], AA: Semigroup[AA]): F[AA] = F.map(value)(_.merge(ev, AA))

  def show(implicit show: Show[F[Ior[A, B]]]): String = show.show(value)

  def map[D](f: B => D)(implicit F: Functor[F]): IorT[F, A, D] = IorT(F.map(value)(_.map(f)))

  def mapK[G[_]](f: F ~> G): IorT[G, A, B] = IorT[G, A, B](f(value))

  def bimap[C, D](fa: A => C, fb: B => D)(implicit F: Functor[F]): IorT[F, C, D] = IorT(F.map(value)(_.bimap(fa, fb)))

  def leftMap[C](f: A => C)(implicit F: Functor[F]): IorT[F, C, B] = IorT(F.map(value)(_.leftMap(f)))

  def leftFlatMap[BB >: B, C](f: A => IorT[F, C, BB])(implicit F: Monad[F], BB: Semigroup[BB]): IorT[F, C, BB] =
    IorT(F.flatMap(value) {
      case Ior.Left(a) => f(a).value
      case r @ Ior.Right(_) => F.pure(r.asInstanceOf[Ior[C, BB]])
      case Ior.Both(a, b) => F.map(f(a).value) {
        case Ior.Left(c) => Ior.Both(c, b)
        case Ior.Right(b1) => Ior.Right(BB.combine(b, b1))
        case Ior.Both(c, b1) => Ior.Both(c, BB.combine(b, b1))
      }
    })

  def leftSemiflatMap[C](f: A => F[C])(implicit F: Monad[F]): IorT[F, C, B] =
    IorT(F.flatMap(value) {
      case Ior.Left(a) => F.map(f(a))(Ior.Left(_))
      case r @ Ior.Right(_) => F.pure(r.asInstanceOf[Ior[C, B]])
      case Ior.Both(a, b) => F.map(f(a))(Ior.Both(_, b))
    })

  def transform[C, D](f: Ior[A, B] => Ior[C, D])(implicit F: Functor[F]): IorT[F, C, D] = IorT(F.map(value)(f))

  def applyAlt[D](ff: IorT[F, A, B => D])(implicit F: Apply[F], A: Semigroup[A]): IorT[F, A, D] =
    IorT[F, A, D](F.map2(value, ff.value)((iorb, iorbd) => Apply[Ior[A, ?]].ap(iorbd)(iorb)))

  def flatMap[AA >: A, D](f: B => IorT[F, AA, D])(implicit F: Monad[F], AA: Semigroup[AA]): IorT[F, AA, D] =
    IorT(F.flatMap(value) {
      case l @ Ior.Left(_) => F.pure(l.asInstanceOf[Ior[AA, D]])
      case Ior.Right(b) => f(b).value
      case Ior.Both(a, b) => F.map(f(b).value) {
        case Ior.Left(a1) => Ior.Left(AA.combine(a, a1))
        case Ior.Right(d) => Ior.Both(a, d)
        case Ior.Both(a1, d) => Ior.Both(AA.combine(a, a1), d)
      }
    })

  def flatMapF[AA >: A, D](f: B => F[Ior[AA, D]])(implicit F: Monad[F], AA: Semigroup[AA]): IorT[F, AA, D] =
    flatMap(f andThen IorT.apply)

  def subflatMap[AA >: A, D](f: B => Ior[AA, D])(implicit F: Functor[F], AA: Semigroup[AA]): IorT[F, AA, D] =
    IorT(F.map(value)(_.flatMap(f)))

  def semiflatMap[D](f: B => F[D])(implicit F: Monad[F]): IorT[F, A, D] =
    IorT(F.flatMap(value) {
      case l @ Ior.Left(_) => F.pure(l.asInstanceOf[Ior[A, D]])
      case Ior.Right(b) => F.map(f(b))(Ior.right)
      case Ior.Both(a, b) => F.map(f(b))(Ior.both(a, _))
    })

  def traverse[G[_], D](f: B => G[D])(implicit traverseF: Traverse[F], applicativeG: Applicative[G]): G[IorT[F, A, D]] =
    applicativeG.map(traverseF.traverse(value)(ior => Traverse[Ior[A, ?]].traverse(ior)(f)))(IorT.apply)

  def foldLeft[C](c: C)(f: (C, B) => C)(implicit F: Foldable[F]): C =
    F.foldLeft(value, c)((c, ior) => ior.foldLeft(c)(f))

  def foldRight[C](lc: Eval[C])(f: (B, Eval[C]) => Eval[C])(implicit F: Foldable[F]): Eval[C] =
    F.foldRight(value, lc)((ior, lc) => ior.foldRight(lc)(f))

  def ===(that: IorT[F, A, B])(implicit eq: Eq[F[Ior[A, B]]]): Boolean =
    eq.eqv(value, that.value)

  def combine(that: IorT[F, A, B])(implicit F: Apply[F], A: Semigroup[A], B: Semigroup[B]): IorT[F, A, B] =
    IorT(F.map2(this.value, that.value)(_ combine _))
}

object IorT extends IorTInstances {

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
   * Same as [[liftF]], but expressed as a FunctionK for use with [[IorT.mapK]]
   * {{{
   * scala> import cats._, data._, implicits._
   * scala> val a: OptionT[Eval, Int] = 1.pure[OptionT[Eval, ?]]
   * scala> val b: OptionT[IorT[Eval, String, ?], Int] = a.mapK(IorT.liftK)
   * scala> b.value.value.value
   * res0: cats.data.Ior[String,Option[Int]] = Right(Some(1))
   * }}}
   */
  final def liftK[F[_], A](implicit F: Functor[F]): F ~> IorT[F, A, ?] =
    new (F ~> IorT[F, A, ?]) {
      def apply[B](fb: F[B]): IorT[F, A, B] = right(fb)
    }

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
   * If the condition is satisfied, return the given `B` in `Ior.Right`, otherwise, return the given
   * `A` in `Ior.Left`, lifted into the specified `Applicative`.
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.implicits._
   * scala> val userInput = "hello world"
   * scala> IorT.cond[Option](
   *      |   userInput.forall(_.isDigit) && userInput.size == 10,
   *      |   userInput,
   *      |   "The input does not look like a phone number")
   * res0: cats.data.IorT[Option,String,String] = IorT(Some(Left(The input does not look like a phone number)))
   * }}}
   */
  final def cond[F[_]]: CondPartiallyApplied[F] = new CondPartiallyApplied[F]

  /**
   * If the condition is satisfied, return the value of `IorT.right` on `F[B]`, otherwise, return the
   * value of `IorT.left` on `F[A]`.
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.implicits._
   * scala> val userInput = "hello world"
   * scala> IorT.condF[Option, String, String](
   *      |   userInput.forall(_.isDigit) && userInput.size == 10,
   *      |   Some(userInput),
   *      |   None)
   * res0: cats.data.IorT[Option,String,String] = IorT(None)
   * }}}
   */
  final def condF[F[_], A, B](test: Boolean, right: => F[B], left: => F[A])(implicit F: Functor[F]): IorT[F, A, B] =
    IorT(if (test) F.map(right)(Ior.right) else F.map(left)(Ior.left))
}

private[data] abstract class IorTInstances extends IorTInstances1 {

  implicit def catsDataShowForIorT[F[_], A, B](implicit sh: Show[F[Ior[A, B]]]): Show[IorT[F, A, B]] =
    Contravariant[Show].contramap(sh)(_.value)

  implicit def catsDataBifunctorForIorT[F[_]](implicit F: Functor[F]): Bifunctor[IorT[F, ?, ?]] =
    new Bifunctor[IorT[F, ?, ?]] {
      override def bimap[A, B, C, D](iort: IorT[F, A, B])(fa: A => C, fb: B => D): IorT[F, C, D] = iort.bimap(fa, fb)
    }

  implicit def catsDataTraverseForIorT[F[_], A](implicit F: Traverse[F]): Traverse[IorT[F, A, ?]] =
    new IorTTraverse[F, A] { val F0: Traverse[F] = F }

  implicit def catsDataMonoidForIorT[F[_], A, B](implicit F: Monoid[F[Ior[A, B]]]): Monoid[IorT[F, A, B]] =
    new IorTMonoid[F, A, B] { val F0: Monoid[F[Ior[A, B]]] = F }

  implicit def catsDataParallelForIorTWithParallelEffect[M[_], F[_], E]
    (implicit P: Parallel[M, F], E: Semigroup[E]): Parallel[IorT[M, E, ?], IorT[F, E, ?]] = new Parallel[IorT[M, E, ?], IorT[F, E, ?]]
  {
    val parallel: IorT[M, E, ?] ~> IorT[F, E, ?] = λ[IorT[M, E, ?] ~> IorT[F, E, ?]](fm => IorT(P.parallel(fm.value)))
    val sequential: IorT[F, E, ?] ~> IorT[M, E, ?] = λ[IorT[F, E, ?] ~> IorT[M, E, ?]](ff => IorT(P.sequential(ff.value)))

    private[this] val FA: Applicative[F] = P.applicative
    private[this] val IorA: Applicative[Ior[E, ?]] = Parallel[Ior[E, ?], Ior[E, ?]].applicative

    val applicative: Applicative[IorT[F, E, ?]] = new Applicative[IorT[F, E, ?]] {
      def pure[A](a: A): IorT[F, E, A] = IorT.pure(a)(FA)
      def ap[A, B](ff: IorT[F, E, A => B])(fa: IorT[F, E, A]): IorT[F, E, B] =
        IorT(FA.map2(ff.value, fa.value)((f, a) => IorA.ap(f)(a)))
    }

    lazy val monad: Monad[IorT[M, E, ?]] = {
      implicit def underlyingMonadM: Monad[M] = P.monad
      Monad[IorT[M, E, ?]]
    }
  }
}

private[data] abstract class IorTInstances1 extends IorTInstances2 {
  implicit def catsDataSemigroupForIorT[F[_], A, B](implicit F: Semigroup[F[Ior[A, B]]]): Semigroup[IorT[F, A, B]] =
    new IorTSemigroup[F, A, B] { val F0: Semigroup[F[Ior[A, B]]] = F }

  implicit def catsDataFoldableForIorT[F[_], A](implicit F: Foldable[F]): Foldable[IorT[F, A, ?]] =
    new IorTFoldable[F, A] { val F0: Foldable[F] = F }

  implicit def catsDataMonadErrorForIorT[F[_], A](implicit F: Monad[F], A: Semigroup[A]): MonadError[IorT[F, A, ?], A] =
    new IorTMonadError[F, A] {
      val A0: Semigroup[A] = A
      val F0: Monad[F] = F
    }

  implicit def catsDataParallelForIorTWithSequentialEffect[F[_], E]
    (implicit F: Monad[F], E: Semigroup[E]): Parallel[IorT[F, E, ?], IorT[F, E, ?]] = new Parallel[IorT[F, E, ?], IorT[F, E, ?]]
  {
    private[this] val identityK: IorT[F, E, ?] ~> IorT[F, E, ?] = FunctionK.id
    private[this] val underlyingParallel: Parallel[Ior[E, ?], Ior[E, ?]] =
      Parallel[Ior[E, ?], Ior[E, ?]]

    def parallel: IorT[F, E, ?] ~> IorT[F, E, ?] = identityK
    def sequential: IorT[F, E, ?] ~> IorT[F, E, ?] = identityK

    val applicative: Applicative[IorT[F, E, ?]] = new Applicative[IorT[F, E, ?]] {
      def pure[A](a: A): IorT[F, E, A] = IorT.pure(a)
      def ap[A, B](ff: IorT[F, E, A => B])(fa: IorT[F, E, A]): IorT[F, E, B] =
        IorT(F.map2(ff.value, fa.value)((f, a) => underlyingParallel.applicative.ap(f)(a)))
    }

    lazy val monad: Monad[IorT[F, E, ?]] = Monad[IorT[F, E, ?]]
  }

}

private[data] abstract class IorTInstances2 extends IorTInstances3 {
  implicit def catsDataMonadErrorFForIorT[F[_], A, E](implicit FE: MonadError[F, E], A: Semigroup[A]): MonadError[IorT[F, A, ?], E] =
    new IorTMonadErrorF[F, A, E] {
      val A0: Semigroup[A] = A
      val F0: MonadError[F, E] = FE
    }

  implicit def catsDataEqForIorT[F[_], A, B](implicit F: Eq[F[Ior[A, B]]]): Eq[IorT[F, A, B]] =
    new IorTEq[F, A, B] { val F0: Eq[F[Ior[A, B]]] = F }
}

private[data] abstract class IorTInstances3 {
  implicit def catsDataFunctorForIorT[F[_], A](implicit F: Functor[F]): Functor[IorT[F, A, ?]] =
    new IorTFunctor[F, A] { val F0: Functor[F] = F }
}

private[data] sealed trait IorTFunctor[F[_], A] extends Functor[IorT[F, A, ?]] {
  implicit def F0: Functor[F]

  override def map[B, D](iort: IorT[F, A, B])(f: B => D): IorT[F, A, D] = iort.map(f)
}

private[data] sealed trait IorTEq[F[_], A, B] extends Eq[IorT[F, A, B]] {
  implicit def F0: Eq[F[Ior[A, B]]]

  override def eqv(x: IorT[F, A, B], y: IorT[F, A, B]): Boolean = x === y
}

private[data] sealed trait IorTMonad[F[_], A] extends Monad[IorT[F, A, ?]] with IorTFunctor[F, A] {
  implicit def A0: Semigroup[A]
  override implicit def F0: Monad[F]

  override def pure[B](b: B): IorT[F, A, B] = IorT.pure(b)

  override def flatMap[B, D](iort: IorT[F, A, B])(f: B => IorT[F, A, D]): IorT[F, A, D] = iort.flatMap(f)

  override def tailRecM[B, D](b: B)(f: B => IorT[F, A, Either[B, D]]): IorT[F, A, D] =
    IorT(F0.tailRecM(Tuple2[B, Option[A]](b, None)) { case (b0, optionA) =>
      F0.map(f(b0).value) {
        case Ior.Left(aa)           => Right(Ior.Left(Semigroup.maybeCombine(optionA, aa)))
        case Ior.Right(Left(b1))    => Left(b1 -> optionA)
        case Ior.Right(Right(d))    => Right(optionA.fold(Ior.right[A, D](d))(Ior.both(_, d)))
        case Ior.Both(aa, Right(d)) => Right(Ior.both(Semigroup.maybeCombine(optionA, aa), d))
        case Ior.Both(aa, Left(b1)) => Left(b1 -> Some(Semigroup.maybeCombine(optionA, aa)))
      }
    })
}

private[data] sealed trait IorTMonadError[F[_], A] extends MonadError[IorT[F, A, ?], A] with IorTMonad[F, A] {
  override def raiseError[B](a: A): IorT[F, A, B] = IorT(F0.pure(Ior.left(a)))

  override def handleErrorWith[B](iort: IorT[F, A, B])(f: A => IorT[F, A, B]): IorT[F, A, B] =
    IorT(F0.flatMap(iort.value) {
      case Ior.Left(a) => f(a).value
      case r @ (Ior.Right(_) | Ior.Both(_, _))  => F0.pure(r)
    })
}

private[data] sealed trait IorTMonadErrorF[F[_], A, E] extends MonadError[IorT[F, A, ?], E] with IorTMonad[F, A] {
  override implicit def F0: MonadError[F, E]

  override def raiseError[B](e: E): IorT[F, A, B] = IorT(F0.raiseError(e))

  override def handleErrorWith[B](iort: IorT[F, A, B])(f: E => IorT[F, A, B]): IorT[F, A, B] =
    IorT(F0.handleErrorWith(iort.value)(f(_).value))
}

private[data] sealed trait IorTSemigroup[F[_], A, B] extends Semigroup[IorT[F, A, B]] {
  implicit def F0: Semigroup[F[Ior[A, B]]]

  override def combine(x: IorT[F, A, B], y: IorT[F, A, B]): IorT[F, A, B] =
    IorT(F0.combine(x.value, y.value))
}

private[data] sealed trait IorTMonoid[F[_], A, B] extends Monoid[IorT[F, A, B]] with IorTSemigroup[F, A, B] {
  override implicit def F0: Monoid[F[Ior[A, B]]]

  override def empty: IorT[F, A, B] = IorT(F0.empty)
}

private[data] sealed trait IorTFoldable[F[_], A] extends Foldable[IorT[F, A, ?]] {
  implicit def F0: Foldable[F]

  override def foldLeft[B, C](iort: IorT[F, A, B], c: C)(f: (C, B) => C): C = iort.foldLeft(c)(f)

  override def foldRight[B, C](iort: IorT[F, A, B], lc: Eval[C])(f: (B, Eval[C]) => Eval[C]): Eval[C] = iort.foldRight(lc)(f)
}

private[data] sealed trait IorTTraverse[F[_], A] extends Traverse[IorT[F, A, ?]] with IorTFoldable[F, A] {
  override implicit def F0: Traverse[F]

  override def traverse[G[_] : Applicative, B, D](iort: IorT[F, A, B])(f: B => G[D]): G[IorT[F, A, D]] = iort.traverse(f)
}
