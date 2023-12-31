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
package data

import cats.arrow.FunctionK

final case class IorT[F[_], A, B](value: F[Ior[A, B]]) {

  def fold[C](fa: A => C, fb: B => C, fab: (A, B) => C)(implicit F: Functor[F]): F[C] =
    F.map(value)(_.fold(fa, fb, fab))

  /**
   * Transform this `IorT[F, A, B]` into a `F[C]`.
   *
   * Example:
   * {{{
   * scala> import cats.data.{Ior, IorT}
   *
   * scala> val iorT: IorT[List, String, Int] = IorT[List, String, Int](List(Ior.Right(123),Ior.Left("abc"), Ior.Both("abc", 123)))
   * scala> iorT.foldF(string => string.split("").toList, int => List(int.toString), (string, int) => string.split("").toList ++ List(int.toString))
   * val res0: List[String] = List(123, a, b, c, a, b, c, 123)
   * }}}
   */
  def foldF[C](fa: A => F[C], fb: B => F[C], fab: (A, B) => F[C])(implicit F: FlatMap[F]): F[C] =
    F.flatMap(value)(_.fold(fa, fb, fab))

  def isLeft(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isLeft)

  def isRight(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isRight)

  def isBoth(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isBoth)

  def swap(implicit F: Functor[F]): IorT[F, B, A] = IorT(F.map(value)(_.swap))

  def getOrElse[BB >: B](default: => BB)(implicit F: Functor[F]): F[BB] = F.map(value)(_.getOrElse(default))

  def getOrElseF[BB >: B](default: => F[BB])(implicit F: Monad[F]): F[BB] =
    F.flatMap(value) {
      case Ior.Left(_)    => default
      case Ior.Right(b)   => F.pure(b)
      case Ior.Both(_, b) => F.pure(b)
    }

  /***
   *
   * Like [[getOrElseF]] but accept an error `E` and raise it when the inner `Ior` is `Left`
   *
   * Equivalent to `getOrElseF(F.raiseError(e)))`
   *
   * Example:
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.syntax.all._
   * scala> import scala.util.{Success, Failure, Try}

   * scala> val iorT: IorT[Try,String,Int] = IorT.leftT("abc")
   * scala> iorT.getOrRaise(new RuntimeException("ERROR!"))
   * res0: Try[Int] = Failure(java.lang.RuntimeException: ERROR!)
   * }}}
   */
  def getOrRaise[E](e: => E)(implicit F: MonadError[F, ? >: E]): F[B] =
    getOrElseF(F.raiseError(e))

  def valueOr[BB >: B](f: A => BB)(implicit F: Functor[F], BB: Semigroup[BB]): F[BB] = F.map(value)(_.valueOr(f))

  def forall(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.forall(f))

  def exists(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.exists(f))

  def toOption(implicit F: Functor[F]): OptionT[F, B] = OptionT(F.map(value)(_.toOption))

  def toEither(implicit F: Functor[F]): EitherT[F, A, B] = EitherT(F.map(value)(_.toEither))

  def toNested: Nested[F, Ior[A, *], B] = Nested[F, Ior[A, *], B](value)

  def toNestedValidated(implicit F: Functor[F]): Nested[F, Validated[A, *], B] =
    Nested[F, Validated[A, *], B](F.map(value)(_.toValidated))

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
      case Ior.Left(a)      => f(a).value
      case r @ Ior.Right(_) => F.pure(r.asInstanceOf[Ior[C, BB]])
      case Ior.Both(a, b) =>
        F.map(f(a).value) {
          case Ior.Left(c)     => Ior.Both(c, b)
          case Ior.Right(b1)   => Ior.Right(BB.combine(b, b1))
          case Ior.Both(c, b1) => Ior.Both(c, BB.combine(b, b1))
        }
    })

  def leftSemiflatMap[C](f: A => F[C])(implicit F: Monad[F]): IorT[F, C, B] =
    IorT(F.flatMap(value) {
      case Ior.Left(a)      => F.map(f(a))(Ior.Left(_))
      case r @ Ior.Right(_) => F.pure(r.asInstanceOf[Ior[C, B]])
      case Ior.Both(a, b)   => F.map(f(a))(Ior.Both(_, b))
    })

  def transform[C, D](f: Ior[A, B] => Ior[C, D])(implicit F: Functor[F]): IorT[F, C, D] = IorT(F.map(value)(f))

  def applyAlt[D](ff: IorT[F, A, B => D])(implicit F: Apply[F], A: Semigroup[A]): IorT[F, A, D] =
    IorT[F, A, D](F.map2(value, ff.value)((iorb, iorbd) => Apply[Ior[A, *]].ap(iorbd)(iorb)))

  def flatMap[AA >: A, D](f: B => IorT[F, AA, D])(implicit F: Monad[F], AA: Semigroup[AA]): IorT[F, AA, D] =
    IorT(F.flatMap(value) {
      case l @ Ior.Left(_) => F.pure(l.asInstanceOf[Ior[AA, D]])
      case Ior.Right(b)    => f(b).value
      case Ior.Both(a, b) =>
        F.map(f(b).value) {
          case Ior.Left(a1)    => Ior.Left(AA.combine(a, a1))
          case Ior.Right(d)    => Ior.Both(a, d)
          case Ior.Both(a1, d) => Ior.Both(AA.combine(a, a1), d)
        }
    })

  def flatMapF[AA >: A, D](f: B => F[Ior[AA, D]])(implicit F: Monad[F], AA: Semigroup[AA]): IorT[F, AA, D] =
    flatMap(b => IorT(f(b)))

  def subflatMap[AA >: A, D](f: B => Ior[AA, D])(implicit F: Functor[F], AA: Semigroup[AA]): IorT[F, AA, D] =
    IorT(F.map(value)(_.flatMap(f)))

  def semiflatMap[D](f: B => F[D])(implicit F: Monad[F]): IorT[F, A, D] =
    IorT(F.flatMap(value) {
      case l @ Ior.Left(_) => F.pure(l.asInstanceOf[Ior[A, D]])
      case Ior.Right(b)    => F.map(f(b))(Ior.right)
      case Ior.Both(a, b)  => F.map(f(b))(Ior.both(a, _))
    })

  def traverse[G[_], D](f: B => G[D])(implicit traverseF: Traverse[F], applicativeG: Applicative[G]): G[IorT[F, A, D]] =
    applicativeG.map(traverseF.traverse(value)(ior => Traverse[Ior[A, *]].traverse(ior)(f)))(IorT.apply)

  def foldLeft[C](c: C)(f: (C, B) => C)(implicit F: Foldable[F]): C =
    F.foldLeft(value, c)((c, ior) => ior.foldLeft(c)(f))

  def foldRight[C](lc: Eval[C])(f: (B, Eval[C]) => Eval[C])(implicit F: Foldable[F]): Eval[C] =
    F.foldRight(value, lc)((ior, lc) => ior.foldRight(lc)(f))

  def ===(that: IorT[F, A, B])(implicit eq: Eq[F[Ior[A, B]]]): Boolean =
    eq.eqv(value, that.value)

  def compare(that: IorT[F, A, B])(implicit ord: Order[F[Ior[A, B]]]): Int =
    ord.compare(value, that.value)

  def combine(that: IorT[F, A, B])(implicit F: Apply[F], A: Semigroup[A], B: Semigroup[B]): IorT[F, A, B] =
    IorT(F.map2(this.value, that.value)(_.combine(_)))
}

object IorT extends IorTInstances {

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class LeftPartiallyApplied[B](private val dummy: Boolean = true) extends AnyVal {
    def apply[F[_], A](fa: F[A])(implicit F: Functor[F]): IorT[F, A, B] = IorT(F.map(fa)(Ior.left))
  }

  /**
   * Creates a left version of `IorT[F, A, B]` from a `F[A]`
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.syntax.all._
   * scala> IorT.left[Int](Option("err"))
   * res0: cats.data.IorT[Option,String,Int] = IorT(Some(Left(err)))
   * }}}
   */
  final def left[B]: LeftPartiallyApplied[B] = new LeftPartiallyApplied[B]

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class LeftTPartiallyApplied[F[_], B](private val dummy: Boolean = true) extends AnyVal {
    def apply[A](a: A)(implicit F: Applicative[F]): IorT[F, A, B] = IorT(F.pure(Ior.left(a)))
  }

  /**
   * Creates a left version of `IorT[F, A, B]` from a `A`
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.syntax.all._
   * scala> IorT.leftT[Option, Int]("err")
   * res0: cats.data.IorT[Option,String,Int] = IorT(Some(Left(err)))
   *
   * }}}
   */
  final def leftT[F[_], B]: LeftTPartiallyApplied[F, B] = new LeftTPartiallyApplied[F, B]

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class RightPartiallyApplied[A](private val dummy: Boolean = true) extends AnyVal {
    def apply[F[_], B](fb: F[B])(implicit F: Functor[F]): IorT[F, A, B] = IorT(F.map(fb)(Ior.right))
  }

  /**
   * Creates a right version of `IorT[F, A, B]` from a `F[B]`
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.syntax.all._
   * scala> IorT.right[String](Option(3))
   * res0: cats.data.IorT[Option,String,Int] = IorT(Some(Right(3)))
   * }}}
   */
  final def right[A]: RightPartiallyApplied[A] = new RightPartiallyApplied[A]

  /**
   * Alias for [[pure]]
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.syntax.all._
   * scala> IorT.rightT[Option, String](3)
   * res0: cats.data.IorT[Option,String,Int] = IorT(Some(Right(3)))
   * }}}
   */
  final def rightT[F[_], A]: PurePartiallyApplied[F, A] = pure

  /**
   * Creates a both version of `IorT[F, A, B]` from a `F[A]` and a `F[B]`
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.syntax.all._
   * scala> IorT.both(Option("err"), Option(3))
   * res0: cats.data.IorT[Option,String,Int] = IorT(Some(Both(err,3)))
   * }}}
   */
  final def both[F[_], A, B](fa: F[A], fb: F[B])(implicit F: Apply[F]): IorT[F, A, B] =
    IorT(F.map2(fa, fb)((a, b) => Ior.Both(a, b)))

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class BothTPartiallyApplied[F[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[A, B](a: A, b: B)(implicit F: Applicative[F]): IorT[F, A, B] = IorT(F.pure(Ior.Both(a, b)))
  }

  /**
   * Creates a both version of `IorT[F, A, B]` from a `A` and a `B`
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.syntax.all._
   * scala> IorT.bothT[Option]("err", 3)
   * res0: cats.data.IorT[Option,String,Int] = IorT(Some(Both(err,3)))
   * }}}
   */
  final def bothT[F[_]]: BothTPartiallyApplied[F] = new BothTPartiallyApplied[F]

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class PurePartiallyApplied[F[_], A](private val dummy: Boolean = true) extends AnyVal {
    def apply[B](b: B)(implicit F: Applicative[F]): IorT[F, A, B] = IorT(F.pure(Ior.right(b)))
  }

  /**
   * Creates a right version of `IorT[F, A, B]` from a `B`
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.syntax.all._
   * scala> IorT.pure[Option, String](3)
   * res0: cats.data.IorT[Option,String,Int] = IorT(Some(Right(3)))
   * }}}
   */
  final def pure[F[_], A]: PurePartiallyApplied[F, A] = new PurePartiallyApplied[F, A]

  /**
   * Alias for [[right]]
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.syntax.all._
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
   * scala> val a: OptionT[Eval, Int] = 1.pure[OptionT[Eval, *]]
   * scala> val b: OptionT[IorT[Eval, String, *], Int] = a.mapK(IorT.liftK)
   * scala> b.value.value.value
   * res0: cats.data.Ior[String,Option[Int]] = Right(Some(1))
   * }}}
   */
  final def liftK[F[_], A](implicit F: Functor[F]): F ~> IorT[F, A, *] =
    new (F ~> IorT[F, A, *]) {
      def apply[B](fb: F[B]): IorT[F, A, B] = right(fb)
    }

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class FromIorPartiallyApplied[F[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[A, B](ior: Ior[A, B])(implicit F: Applicative[F]): IorT[F, A, B] = IorT(F.pure(ior))
  }

  /**
   * Transforms an `Ior` into an `IorT`, lifted into the specified `Applicative`.
   * {{{
   * scala> import cats.data.{IorT, Ior}
   * scala> import cats.syntax.all._
   * scala> val i: Ior[String, Int] = Ior.both("warning", 3)
   * scala> IorT.fromIor[Option](i)
   * res0: cats.data.IorT[Option,String,Int] = IorT(Some(Both(warning,3)))
   * }}}
   */
  final def fromIor[F[_]]: FromIorPartiallyApplied[F] = new FromIorPartiallyApplied[F]

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class FromEitherPartiallyApplied[F[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](either: Either[E, A])(implicit F: Applicative[F]): IorT[F, E, A] =
      IorT(F.pure(Ior.fromEither(either)))
  }

  /**
   * Transforms an `Either` into an `IorT`, lifted into the specified `Applicative`.
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.syntax.all._
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
   * scala> import cats.syntax.all._
   * scala> val e: Either[String, Int] = Either.right(3)
   * scala> IorT.fromEitherF(Option(e))
   * res0: cats.data.IorT[Option,String,Int] = IorT(Some(Right(3)))
   * }}}
   */
  final def fromEitherF[F[_], E, A](feither: F[Either[E, A]])(implicit F: Functor[F]): IorT[F, E, A] =
    IorT(F.map(feither)(Ior.fromEither))

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class FromOptionPartiallyApplied[F[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](option: Option[A], ifNone: => E)(implicit F: Applicative[F]): IorT[F, E, A] =
      IorT(F.pure(option.fold[Ior[E, A]](Ior.left(ifNone))(Ior.right)))
  }

  /**
   * Transforms an `Option` into an `IorT`, lifted into the specified `Applicative` and using
   * the second argument if the `Option` is a `None`.
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.syntax.all._
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
   * scala> import cats.syntax.all._
   * scala> val o: Option[Int] = None
   * scala> IorT.fromOptionF(List(o), "Answer not known.")
   * res0: cats.data.IorT[List,String,Int]  = IorT(List(Left(Answer not known.)))
   * scala> IorT.fromOptionF(List(Option(42)), "Answer not known.")
   * res1: cats.data.IorT[List,String,Int] = IorT(List(Right(42)))
   * }}}
   */
  final def fromOptionF[F[_], E, A](foption: F[Option[A]], ifNone: => E)(implicit F: Functor[F]): IorT[F, E, A] =
    IorT(F.map(foption)(_.fold[Ior[E, A]](Ior.left(ifNone))(Ior.right)))

  /**
   * Similar to `fromOptionF` but the left is carried from monadic `F[_]` context when the option is `None`
   */
  final def fromOptionM[F[_], E, A](foption: F[Option[A]], ifNone: => F[E])(implicit F: Monad[F]): IorT[F, E, A] =
    IorT(
      F.flatMap(foption) {
        case Some(a) => F.pure(Ior.right[E, A](a))
        case None    => F.map(ifNone)(Ior.left[E, A])
      }
    )

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class CondPartiallyApplied[F[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[A, B](test: Boolean, right: => B, left: => A)(implicit F: Applicative[F]): IorT[F, A, B] =
      IorT(F.pure(if (test) Ior.right(right) else Ior.left(left)))
  }

  /**
   * If the condition is satisfied, return the given `B` in `Ior.Right`, otherwise, return the given
   * `A` in `Ior.Left`, lifted into the specified `Applicative`.
   * {{{
   * scala> import cats.data.IorT
   * scala> import cats.syntax.all._
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
   * scala> import cats.syntax.all._
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

abstract private[data] class IorTInstances extends IorTInstances1 {

  implicit def catsDataShowForIorT[F[_], A, B](implicit sh: Show[F[Ior[A, B]]]): Show[IorT[F, A, B]] =
    Contravariant[Show].contramap(sh)(_.value)

  implicit def catsDataBifunctorForIorT[F[_]](implicit F: Functor[F]): Bifunctor[IorT[F, *, *]] =
    new Bifunctor[IorT[F, *, *]] {
      override def bimap[A, B, C, D](iort: IorT[F, A, B])(fa: A => C, fb: B => D): IorT[F, C, D] = iort.bimap(fa, fb)
    }

  implicit def catsDataTraverseForIorT[F[_], A](implicit F: Traverse[F]): Traverse[IorT[F, A, *]] =
    new IorTTraverse[F, A] with IorTFunctor[F, A] { val F0: Traverse[F] = F }

  implicit def catsDataMonoidForIorT[F[_], A, B](implicit F: Monoid[F[Ior[A, B]]]): Monoid[IorT[F, A, B]] =
    new IorTMonoid[F, A, B] { val F0: Monoid[F[Ior[A, B]]] = F }

  /**
   * An alternative [[Parallel]] implementation which merges the semantics of
   * the outer Parallel (the F[_] effect) with the effects of the inner
   * one (the Ior). The inner Parallel has the semantics of [[Ior]]'s Parallel,
   * while the outer has the semantics of parallel ''evaluation'' (in most cases).
   * The default Parallel for [[IorT]], when the nested F also has a Parallel,
   * is to strictly take the semantics of the nested F and to short-circuit any
   * lefts (often, errors) in a left-to-right fashion, mirroring the semantics of
   * [[Applicative]] on IorT. This instance is different in that it will not
   * ''short-circuit'' but instead accumulate all lefts according to the supplied
   * [[Semigroup]].
   *
   * {{{
   * implicit val p: Parallel[IorT[IO, Chain[Error], *]] = IorT.accumulatingParallel
   *
   * val a = IorT(IO(Chain(error1).leftIor[Unit]))
   * val b = IorT(IO(Chain(error2).leftIor[Unit]))
   *
   * (a, b).parTupled  // => IorT(IO(Chain(error1, error2).leftIor[Unit]))
   * }}}
   */
  def accumulatingParallel[M[_], E](implicit
    P: Parallel[M],
    E: Semigroup[E]
  ): Parallel.Aux[IorT[M, E, *], IorT[P.F, E, *]] =
    new Parallel[IorT[M, E, *]] {
      type F[x] = IorT[P.F, E, x]

      val parallel: IorT[M, E, *] ~> IorT[P.F, E, *] =
        new (IorT[M, E, *] ~> IorT[P.F, E, *]) {
          def apply[A](fm: IorT[M, E, A]): IorT[P.F, E, A] = IorT(P.parallel(fm.value))
        }
      val sequential: IorT[P.F, E, *] ~> IorT[M, E, *] =
        new (IorT[P.F, E, *] ~> IorT[M, E, *]) {
          def apply[A](ff: IorT[P.F, E, A]): IorT[M, E, A] = IorT(P.sequential(ff.value))
        }

      private[this] val FA: Applicative[P.F] = P.applicative
      private[this] val IorA: Applicative[Ior[E, *]] = Parallel[Ior[E, *], Ior[E, *]].applicative

      val applicative: Applicative[IorT[P.F, E, *]] = new Applicative[IorT[P.F, E, *]] {
        def pure[A](a: A): IorT[P.F, E, A] = IorT.pure(a)(FA)
        def ap[A, B](ff: IorT[P.F, E, A => B])(fa: IorT[P.F, E, A]): IorT[P.F, E, B] =
          IorT(FA.map2(ff.value, fa.value)((f, a) => IorA.ap(f)(a)))
      }

      lazy val monad: Monad[IorT[M, E, *]] = {
        implicit def underlyingMonadM: Monad[M] = P.monad
        Monad[IorT[M, E, *]]
      }
    }

  implicit def catsDataParallelForIorTWithParallelEffect[M[_], E](implicit
    P: Parallel[M],
    E: Semigroup[E]
  ): Parallel.Aux[IorT[M, E, *], IorT[P.F, E, *]] { type Dummy } =
    new Parallel[IorT[M, E, *]] {
      type F[x] = IorT[P.F, E, x]
      type Dummy // fix to make this one more specific than the catsDataParallelForIorTWithSequentialEffect, see https://github.com/typelevel/cats/pull/2335#issuecomment-408249775

      val parallel: IorT[M, E, *] ~> IorT[P.F, E, *] =
        new (IorT[M, E, *] ~> IorT[P.F, E, *]) {
          def apply[A](fm: IorT[M, E, A]): IorT[P.F, E, A] = IorT(P.parallel(fm.value))
        }
      val sequential: IorT[P.F, E, *] ~> IorT[M, E, *] =
        new (IorT[P.F, E, *] ~> IorT[M, E, *]) {
          def apply[A](ff: IorT[P.F, E, A]): IorT[M, E, A] = IorT(P.sequential(ff.value))
        }

      private[this] val FA: Applicative[P.F] = P.applicative
      private[this] val IorA: Applicative[Ior[E, *]] =
        Ior.catsDataMonadErrorForIor // See https://github.com/typelevel/cats/issues/3783

      val applicative: Applicative[IorT[P.F, E, *]] = new Applicative[IorT[P.F, E, *]] {
        def pure[A](a: A): IorT[P.F, E, A] = IorT.pure(a)(FA)
        def ap[A, B](ff: IorT[P.F, E, A => B])(fa: IorT[P.F, E, A]): IorT[P.F, E, B] =
          IorT(FA.map2(ff.value, fa.value)((f, a) => IorA.ap(f)(a)))
      }

      lazy val monad: Monad[IorT[M, E, *]] = {
        implicit def underlyingMonadM: Monad[M] = P.monad
        Monad[IorT[M, E, *]]
      }
    }

  implicit def catsDataDeferForIor[F[_], E](implicit F: Defer[F]): Defer[IorT[F, E, *]] =
    new Defer[IorT[F, E, *]] {
      def defer[A](fa: => IorT[F, E, A]): IorT[F, E, A] =
        IorT(F.defer(fa.value))
    }
}

abstract private[data] class IorTInstances1 extends IorTInstances2 {
  implicit def catsDataSemigroupForIorT[F[_], A, B](implicit F: Semigroup[F[Ior[A, B]]]): Semigroup[IorT[F, A, B]] =
    new IorTSemigroup[F, A, B] { val F0: Semigroup[F[Ior[A, B]]] = F }

  implicit def catsDataFoldableForIorT[F[_], A](implicit F: Foldable[F]): Foldable[IorT[F, A, *]] =
    new IorTFoldable[F, A] { val F0: Foldable[F] = F }

  implicit def catsDataMonadErrorForIorT[F[_], A](implicit F: Monad[F], A: Semigroup[A]): MonadError[IorT[F, A, *], A] =
    new IorTMonadError[F, A] {
      val A0: Semigroup[A] = A
      val F0: Monad[F] = F
    }

  implicit def catsDataParallelForIorTWithSequentialEffect[F0[_], E](implicit
    F: Monad[F0],
    E: Semigroup[E]
  ): Parallel.Aux[IorT[F0, E, *], IorT[F0, E, *]] =
    new Parallel[IorT[F0, E, *]] {
      type F[x] = IorT[F0, E, x]
      private[this] val identityK: IorT[F0, E, *] ~> IorT[F0, E, *] = FunctionK.id
      private[this] val underlyingParallel: Parallel.Aux[Ior[E, *], Ior[E, *]] =
        Ior.catsDataParallelForIor[E]

      def parallel: IorT[F0, E, *] ~> IorT[F0, E, *] = identityK
      def sequential: IorT[F0, E, *] ~> IorT[F0, E, *] = identityK

      val applicative: Applicative[IorT[F0, E, *]] = new Applicative[IorT[F0, E, *]] {
        def pure[A](a: A): IorT[F0, E, A] = IorT.pure(a)
        def ap[A, B](ff: IorT[F0, E, A => B])(fa: IorT[F0, E, A]): IorT[F0, E, B] =
          IorT(F.map2(ff.value, fa.value)((f, a) => underlyingParallel.applicative.ap[A, B](f)(a)))
      }

      lazy val monad: Monad[IorT[F0, E, *]] = Monad[IorT[F0, E, *]]
    }

  implicit def catsDataOrderForIorT[F[_], A, B](implicit F: Order[F[Ior[A, B]]]): Order[IorT[F, A, B]] =
    new IorTOrder[F, A, B] { val F0: Order[F[Ior[A, B]]] = F }

}

abstract private[data] class IorTInstances2 extends IorTInstances3 {
  implicit def catsDataMonadErrorFForIorT[F[_], A, E](implicit
    FE: MonadError[F, E],
    A: Semigroup[A]
  ): MonadError[IorT[F, A, *], E] =
    new IorTMonadErrorF[F, A, E] {
      val A0: Semigroup[A] = A
      val F0: MonadError[F, E] = FE
    }

  implicit def catsDataEqForIorT[F[_], A, B](implicit F: Eq[F[Ior[A, B]]]): Eq[IorT[F, A, B]] =
    new IorTEq[F, A, B] { val F0: Eq[F[Ior[A, B]]] = F }
}

abstract private[data] class IorTInstances3 {
  implicit def catsDataFunctorForIorT[F[_], A](implicit F: Functor[F]): Functor[IorT[F, A, *]] =
    new IorTFunctor[F, A] { val F0: Functor[F] = F }
}

sealed private[data] trait IorTFunctor[F[_], A] extends Functor[IorT[F, A, *]] {
  implicit def F0: Functor[F]

  override def map[B, D](iort: IorT[F, A, B])(f: B => D): IorT[F, A, D] = iort.map(f)
}

sealed private[data] trait IorTEq[F[_], A, B] extends Eq[IorT[F, A, B]] {
  implicit def F0: Eq[F[Ior[A, B]]]

  override def eqv(x: IorT[F, A, B], y: IorT[F, A, B]): Boolean = x === y
}

sealed private[data] trait IorTOrder[F[_], A, B] extends Order[IorT[F, A, B]] {
  implicit def F0: Order[F[Ior[A, B]]]

  override def compare(x: IorT[F, A, B], y: IorT[F, A, B]): Int = x.compare(y)
}

sealed private[data] trait IorTMonad[F[_], A] extends Monad[IorT[F, A, *]] with IorTFunctor[F, A] {
  implicit def A0: Semigroup[A]
  implicit override def F0: Monad[F]

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

sealed private[data] trait IorTMonadError[F[_], A] extends MonadError[IorT[F, A, *], A] with IorTMonad[F, A] {
  override def raiseError[B](a: A): IorT[F, A, B] = IorT(F0.pure(Ior.left(a)))

  override def handleErrorWith[B](iort: IorT[F, A, B])(f: A => IorT[F, A, B]): IorT[F, A, B] =
    IorT(F0.flatMap(iort.value) {
      case Ior.Left(a)                         => f(a).value
      case r @ (Ior.Right(_) | Ior.Both(_, _)) => F0.pure(r)
    })
}

sealed private[data] trait IorTMonadErrorF[F[_], A, E] extends MonadError[IorT[F, A, *], E] with IorTMonad[F, A] {
  implicit override def F0: MonadError[F, E]

  override def raiseError[B](e: E): IorT[F, A, B] = IorT(F0.raiseError(e))

  override def handleErrorWith[B](iort: IorT[F, A, B])(f: E => IorT[F, A, B]): IorT[F, A, B] =
    IorT(F0.handleErrorWith(iort.value)(f(_).value))
}

sealed private[data] trait IorTSemigroup[F[_], A, B] extends Semigroup[IorT[F, A, B]] {
  implicit def F0: Semigroup[F[Ior[A, B]]]

  override def combine(x: IorT[F, A, B], y: IorT[F, A, B]): IorT[F, A, B] =
    IorT(F0.combine(x.value, y.value))
}

sealed private[data] trait IorTMonoid[F[_], A, B] extends Monoid[IorT[F, A, B]] with IorTSemigroup[F, A, B] {
  implicit override def F0: Monoid[F[Ior[A, B]]]

  override def empty: IorT[F, A, B] = IorT(F0.empty)
}

sealed private[data] trait IorTFoldable[F[_], A] extends Foldable[IorT[F, A, *]] {
  implicit def F0: Foldable[F]

  override def foldLeft[B, C](iort: IorT[F, A, B], c: C)(f: (C, B) => C): C = iort.foldLeft(c)(f)

  override def foldRight[B, C](iort: IorT[F, A, B], lc: Eval[C])(f: (B, Eval[C]) => Eval[C]): Eval[C] =
    iort.foldRight(lc)(f)
}

sealed private[data] trait IorTTraverse[F[_], A] extends Traverse[IorT[F, A, *]] with IorTFoldable[F, A] {
  implicit override def F0: Traverse[F]

  override def traverse[G[_]: Applicative, B, D](iort: IorT[F, A, B])(f: B => G[D]): G[IorT[F, A, D]] = iort.traverse(f)
}
