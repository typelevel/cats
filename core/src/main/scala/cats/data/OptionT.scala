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

/**
 * `OptionT[F[_], A]` is a light wrapper on an `F[Option[A]]` with some
 * convenient methods for working with this nested structure.
 *
 * It may also be said that `OptionT` is a monad transformer for `Option`.
 *
 * For more information, see the [[http://typelevel.org/cats/datatypes/optiont.html documentation]].
 */
final case class OptionT[F[_], A](value: F[Option[A]]) {

  def fold[B](default: => B)(f: A => B)(implicit F: Functor[F]): F[B] =
    F.map(value)(_.fold(default)(f))

  /**
   * Transform this `OptionT[F, A]` into a `F[B]`.
   *
   * Example:
   * {{{
   * scala> import cats.data.OptionT
   *
   * scala> val optionT: OptionT[List, Int] = OptionT[List, Int](List(Some(23), None))
   * scala> optionT.foldF(Nil)(v => List(v, v * 2))
   * res0: List[Int] = List(23, 46)
   * }}}
   */
  def foldF[B](default: => F[B])(f: A => F[B])(implicit F: FlatMap[F]): F[B] =
    F.flatMap(value)(_.fold(default)(f))

  /**
   * Transform this `OptionT[F, A]` into a `F[Unit]`.
   * This is identical to `foldF(F.unit)(f)`.
   */
  def foreachF(f: A => F[Unit])(implicit F: Monad[F]): F[Unit] =
    foldF(F.unit)(f)

  /**
   * Catamorphism on the Option. This is identical to [[fold]], but it only has
   * one parameter list, which can result in better type inference in some
   * contexts.
   *
   * Example:
   * {{{
   * scala> import cats.data.OptionT
   *
   * scala> val optionT: OptionT[List, Int] = OptionT[List, Int](List(Some(42), None))
   * scala> optionT.cata[String]("default", x => x.toString + "!")
   * res0: List[String] = List(42!, default)
   * }}}
   */
  def cata[B](default: => B, f: A => B)(implicit F: Functor[F]): F[B] =
    fold(default)(f)

  /**
   * Effectful catamorphism on the Option. This is identical to [[foldF]], but it only has
   * one parameter list, which can result in better type inference in some
   * contexts.
   *
   * Example:
   * {{{
   * scala> import cats.data.OptionT
   *
   * scala> val optionT: OptionT[List, Int] = OptionT[List, Int](List(Some(42), None))
   * scala> optionT.cataF[String](Nil, x => List(x.toString + "!"))
   * res0: List[String] = List(42!)
   * }}}
   */
  def cataF[B](default: => F[B], f: A => F[B])(implicit F: FlatMap[F]): F[B] =
    foldF(default)(f)

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT(List(Some(2), None, Some(414), None, None))
   *  scala> optionT.map(_.toString + "!")
   *  res0: OptionT[List, String] = OptionT(List(Some(2!), None, Some(414!), None, None))
   * }}}
   */
  def map[B](f: A => B)(implicit F: Functor[F]): OptionT[F, B] =
    OptionT(F.map(value)(_.map(f)))

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT.some[List](99)
   *  scala> optionT.imap[Char](_.toChar)(_.toInt)
   *  res0: OptionT[List, Char] = OptionT(List(Some(c)))
   * }}}
   */
  def imap[B](f: A => B)(g: B => A)(implicit F: Invariant[F]): OptionT[F, B] =
    OptionT {
      F.imap(value)(_.map(f))(_.map(g))
    }

  /**
   * Example:
   * {{{
   *  scala> import cats.Show
   *  scala> import cats.data.OptionT
   *
   *  scala> val showIntOption: Show[Option[Int]] = oi => oi.map(_.toString + "!").getOrElse("default")
   *  scala> val optionT: OptionT[Show, Int] = OptionT[Show, Int](showIntOption)
   *  scala> optionT.contramap[Double](_.toInt).value.show(Some(5.4321))
   *  res0: String = 5!
   * }}}
   */
  def contramap[B](f: B => A)(implicit F: Contravariant[F]): OptionT[F, B] =
    OptionT {
      F.contramap(value)(_.map(f))
    }

  /**
   * Modify the context `F` using transformation `f`.
   * 
   * Example:
   * {{{
   *  scala> import cats.~>
   *  scala> import cats.data.OptionT
   * 
   *  scala> val optionToList: Option ~> List = new ~>[Option, List] { override def apply[A](o: Option[A]): List[A] = o.toList }
   *  scala> val optionT: OptionT[Option, Int] = OptionT.some(42)
   *  scala> optionT.mapK[List](optionToList)
   *  res0: OptionT[List, Int] = OptionT(List(Some(42)))
   * }}}
   */
  def mapK[G[_]](f: F ~> G): OptionT[G, A] = OptionT[G, A](f(value))

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT[List, Int](List(Some(3), Some(5)))
   *  scala> optionT.semiflatMap(x => List(x, x * 6))
   *  res0: OptionT[List, Int] = OptionT(List(Some(3), Some(18), Some(5), Some(30)))
   * }}}
   */
  def semiflatMap[B](f: A => F[B])(implicit F: Monad[F]): OptionT[F, B] =
    flatMap(a => OptionT.liftF(f(a)))

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[Either[String, *], Int] = OptionT.some[Either[String, *]](3)
   *  scala> optionT.semiflatTap { case 1 | 2 | 3 => Right("hit!"); case _ => Left("miss!") }
   *  res0: OptionT[Either[String, *], Int] = OptionT(Right(Some(3)))
   *  scala> optionT.semiflatTap { case 0 | 1 | 2 => Right("hit!"); case _ => Left("miss!") }
   *  res1: OptionT[Either[String, *], Int] = OptionT(Left(miss!))
   * }}}
   */
  def semiflatTap[B](f: A => F[B])(implicit F: Monad[F]): OptionT[F, A] =
    semiflatMap(a => F.as(f(a), a))

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT[List, Int](List(Some(2), Some(3), Some(4)))
   *  scala> optionT.mapFilter(x => Option(x).filter(_ % 2 == 0))
   *  res0: OptionT[List, Int] = OptionT(List(Some(2), None, Some(4)))
   * }}}
   */
  def mapFilter[B](f: A => Option[B])(implicit F: Functor[F]): OptionT[F, B] =
    subflatMap(f)

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT[List, Int](List(Some(2), Some(3), Some(4)))
   *  scala> optionT.flatMap(x => OptionT.when(x % 2 == 0)(x))
   *  res0: OptionT[List, Int] = OptionT(List(Some(2), None, Some(4)))
   * }}}
   */
  def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] =
    flatMapF(a => f(a).value)

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT[List, Int](List(Some(2), Some(3), Some(4)))
   *  scala> optionT.flatMapF(x => List(Option(x).filter(_ % 2 == 0)))
   *  res0: OptionT[List, Int] = OptionT(List(Some(2), None, Some(4)))
   * }}}
   */
  def flatMapF[B](f: A => F[Option[B]])(implicit F: Monad[F]): OptionT[F, B] =
    OptionT(F.flatMap(value)(_.fold(F.pure[Option[B]](None))(f)))

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT[List, Int](List(Some(2), Some(3), Some(4)))
   *  scala> optionT.flatTransform(x => List(x.filter(_ % 2 == 0)))
   *  res0: OptionT[List, Int] = OptionT(List(Some(2), None, Some(4)))
   * }}}
   */
  def flatTransform[B](f: Option[A] => F[Option[B]])(implicit F: Monad[F]): OptionT[F, B] =
    OptionT(F.flatMap(value)(f))

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT[List, Int](List(Some(2), Some(3), Some(4)))
   *  scala> optionT.transform(_.filter(_ % 2 == 0))
   *  res0: OptionT[List, Int] = OptionT(List(Some(2), None, Some(4)))
   * }}}
   */
  def transform[B](f: Option[A] => Option[B])(implicit F: Functor[F]): OptionT[F, B] =
    OptionT(F.map(value)(f))

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT[List, Int](List(Some(2), Some(3), Some(4)))
   *  scala> optionT.subflatMap(x => Option(x).filter(_ % 2 == 0))
   *  res0: OptionT[List, Int] = OptionT(List(Some(2), None, Some(4)))
   * }}}
   */
  def subflatMap[B](f: A => Option[B])(implicit F: Functor[F]): OptionT[F, B] =
    transform(_.flatMap(f))

  /**
   * Perform an effect if the value inside the is a `None`, leaving the value untouched. Equivalent to [[orElseF]]
   * with an effect returning `None` as argument.
   * 
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> // prints "no value"
   *  scala> val optionT: OptionT[Either[String, *], Int] = OptionT[Either[String, *], Int](Right(None))
   *  scala> optionT.flatTapNone(Left("no value!"))
   *  res0: OptionT[Either[String, *], Int] = OptionT(Left(no value!))
   * }}}
   */
  def flatTapNone[B](ifNone: => F[B])(implicit F: Monad[F]): OptionT[F, A] =
    OptionT(F.flatTap(value)(_.fold(F.void(ifNone))(_ => F.unit)))

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT[List, Int](List(Some(2), None, Some(4)))
   *  scala> optionT.getOrElse(42)
   *  res0: List[Int] = List(2, 42, 4)
   * }}}
   */
  def getOrElse[B >: A](default: => B)(implicit F: Functor[F]): F[B] =
    F.map(value)(_.getOrElse(default))

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT[List, Int](List(Some(2), None, Some(4)))
   *  scala> optionT.getOrElseF(List(42))
   *  res0: List[Int] = List(2, 42, 4)
   * }}}
   */
  def getOrElseF[B >: A](default: => F[B])(implicit F: Monad[F]): F[B] =
    F.flatMap(value)(_.fold(default)(F.pure))

  /**
   * Like [[getOrElseF]] but accept an error `E` and raise it when the inner `Option` is `None`
   * 
   * Equivalent to `getOrElseF(F.raiseError(e)))`
   * 
   * {{{
   * scala> import cats.data.OptionT
   * scala> import scala.util.{Success, Try}
   *
   * scala> val optionT: OptionT[Try, Int] = OptionT[Try, Int](Success(None))
   * scala> optionT.getOrRaise(new RuntimeException("ERROR!"))
   * res0: Try[Int] = Failure(java.lang.RuntimeException: ERROR!)
   * }}}
   */
  def getOrRaise[E](e: => E)(implicit F: MonadError[F, ? >: E]): F[A] =
    getOrElseF(F.raiseError(e))

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT(List(Some(2), None, Some(414), None, None))
   *  scala> optionT.collect{ case i if i == 2 => i }
   *  res0: OptionT[List, Int] = OptionT(List(Some(2), None, None, None, None))
   *  scala> optionT.collect{ case i: Int => i == 2 }
   *  res0: OptionT[List, Boolean] = OptionT(List(Some(true), None, Some(false), None, None))
   * }}}
   */
  def collect[B](f: PartialFunction[A, B])(implicit F: Functor[F]): OptionT[F, B] =
    OptionT(F.map(value)(_.collect(f)))

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT[List, Int](List(Some(2), None, Some(3)))
   *  scala> optionT.exists(_ % 2 == 0)
   *  res0: List[Boolean] = List(true, false, false)
   * }}}
   */
  def exists(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_.exists(f))

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT(List(Some(2), None, Some(414), None, None))
   *  scala> optionT.filter(el => (el % 2 == 0))
   *  res0: OptionT[List, Int] = OptionT(List(Some(2), None, Some(414), None, None))
   *
   *  scala> optionT.filter(el => (el % 3 == 0))
   *  res1: OptionT[List, Int] = OptionT(List(None, None, Some(414), None, None))
   * }}}
   */
  def filter(p: A => Boolean)(implicit F: Functor[F]): OptionT[F, A] =
    OptionT(F.map(value)(_.filter(p)))

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT[List, Int](List(Some(100), None, Some(421), Some(333)))
   *  scala> optionT.filterF(n => List(n % 100 == 0, n.toString.toSet.size == 1))
   *
   *  res0: OptionT[List, Int] = OptionT(List(Some(100), None, None, None, None, None, Some(333)))
   * }}}
   */
  def filterF(p: A => F[Boolean])(implicit F: Monad[F]): OptionT[F, A] =
    OptionT(F.flatMap(value) {
      case v @ Some(a) => F.map(p(a)) { if (_) v else None }
      case None        => F.pure(None)
    })

  /**
   * It is used for desugaring 'for comprehensions'. OptionT wouldn't work in 'for-comprehensions' without
   * this method.
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT(List(Some(2), None, Some(414), None, None))
   *  scala> optionT.withFilter(el => (el % 2 == 0))
   *  res0: OptionT[List, Int] = OptionT(List(Some(2), None, Some(414), None, None))
   *
   *  scala> optionT.withFilter(el => (el % 3 == 0))
   *  res1: OptionT[List, Int] = OptionT(List(None, None, Some(414), None, None))
   * }}}
   */
  def withFilter(p: A => Boolean)(implicit F: Functor[F]): OptionT[F, A] =
    filter(p)(F)

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT(List(Some(2), None, Some(414), None, None))
   *  scala> optionT.filterNot(el => (el % 2 == 0))
   *  res0: OptionT[List, Int] = OptionT(List(None, None, None, None, None))
   *
   *  scala> optionT.filterNot(el => (el % 3 == 0))
   *  res1: OptionT[List, Int] = OptionT(List(Some(2), None, None, None, None))
   * }}}
   */
  def filterNot(p: A => Boolean)(implicit F: Functor[F]): OptionT[F, A] =
    OptionT(F.map(value)(_.filterNot(p)))

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT[List, Int](List(Some(2), None, Some(3)))
   *  scala> optionT.forall(_ % 2 == 0)
   *  res0: List[Boolean] = List(true, true, false)
   * }}}
   */
  def forall(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_.forall(f))

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT(List(Some(2), None, Some(414), None, None))
   *  scala> optionT.isDefined
   *  res0: List[Boolean] = List(true, false, true, false, false)
   * }}}
   */
  def isDefined(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_.isDefined)

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT(List(Some(2), None, Some(414), None, None))
   *  scala> optionT.isEmpty
   *  res0: List[Boolean] = List(false, true, false, true, true)
   * }}}
   */
  def isEmpty(implicit F: Functor[F]): F[Boolean] =
    F.map(value)(_.isEmpty)

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT(List(Some(2), None, Some(414), None, None))
   *  scala> optionT.orElseF(List[Option[Int]](Some(-1)))
   *  res0: OptionT[List, Int] = OptionT(List(Some(2), Some(-1), Some(414), Some(-1), Some(-1)))
   * }}}
   */
  def orElse(default: => OptionT[F, A])(implicit F: Monad[F]): OptionT[F, A] =
    orElseF(default.value)

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT(List(Some(2), None, Some(4)))
   *  scala> optionT.orElseF(List(Some(3)))
   *  res0: OptionT[List, Int] = OptionT(List(Some(2), Some(3), Some(4)))
   * }}}
   */
  def orElseF(default: => F[Option[A]])(implicit F: Monad[F]): OptionT[F, A] =
    OptionT(F.flatMap(value) {
      case s @ Some(_) => F.pure(s)
      case None        => default
    })

  /**
   * Example:
   * {{{
   *  scala> import cats.data.EitherT
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT(List(Some(2), None, Some(4)))
   *  scala> optionT.toRight[Int](3)
   *  res0: EitherT[List, Int, Int] = EitherT(List(Right(2), Left(3), Right(4)))
   * }}}
   */
  def toRight[L](left: => L)(implicit F: Functor[F]): EitherT[F, L, A] =
    EitherT(cata(Left(left), Right.apply))

  /**
   * Example:
   * {{{
   *  scala> import cats.data.EitherT
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT(List(Some(2), None, Some(4)))
   *  scala> optionT.toRightF[Int](List(3))
   *  res0: EitherT[List, Int, Int] = EitherT(List(Right(2), Left(3), Right(4)))
   * }}}
   */
  def toRightF[L](left: => F[L])(implicit F: Monad[F]): EitherT[F, L, A] =
    EitherT(cataF(F.map(left)(Left.apply[L, A]), a => F.pure(Right(a))))

  /**
   * Example:
   * {{{
   *  scala> import cats.data.EitherT
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT(List(Some(2), None, Some(4)))
   *  scala> optionT.toLeft[Int](3)
   *  res0: EitherT[List, Int, Int] = EitherT(List(Left(2), Right(3), Left(4)))
   * }}}
   */
  def toLeft[R](right: => R)(implicit F: Functor[F]): EitherT[F, A, R] =
    EitherT(cata(Right(right), Left.apply))

  /**
   * Example:
   * {{{
   *  scala> import cats.data.EitherT
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT(List(Some(2), None, Some(4)))
   *  scala> optionT.toLeftF[Int](List(3))
   *  res0: EitherT[List, Int, Int] = EitherT(List(Left(2), Right(3), Left(4)))
   * }}}
   */
  def toLeftF[R](right: => F[R])(implicit F: Monad[F]): EitherT[F, A, R] =
    EitherT(cataF(F.map(right)(Right.apply[A, R]), a => F.pure(Left(a))))

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT(List(Some(2), None, Some(414), None, None))
   *  scala> optionT.show
   *  res0: String = List(Some(2), None, Some(414), None, None)
   * }}}
   */
  def show(implicit F: Show[F[Option[A]]]): String = F.show(value)

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT(List(Some(2), None, Some(414), None, None))
   *  scala> optionT.compare(OptionT[List, Int](List(Some(2))))
   *  res0: Int = 1
   *
   *  scala> optionT.compare(OptionT[List, Int](List(Some(2), None, Some(414), None, None)))
   *  res0: Int = 0
   * }}}
   */
  def compare(that: OptionT[F, A])(implicit o: Order[F[Option[A]]]): Int =
    o.compare(value, that.value)

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Double] = OptionT(List(Some(0.1), None, Some(0.5)))
   *  scala> optionT.partialCompare(OptionT[List, Double](List(Some(2))))
   *  res0: Double = -1.0
   *
   *  scala> optionT.partialCompare(OptionT[List, Double](List(Some(0.1), None, Some(0.5))))
   *  res0: Double = 0.0
   * }}}
   */
  def partialCompare(that: OptionT[F, A])(implicit p: PartialOrder[F[Option[A]]]): Double =
    p.partialCompare(value, that.value)

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT(List(Some(2), None, Some(5)))
   *  scala> optionT === OptionT[List, Int](List(Some(2), None))
   *  res0: Boolean = false
   *
   *  scala> optionT === OptionT[List, Int](List(Some(2), None, Some(5)))
   *  res0: Boolean = true
   * }}}
   */
  def ===(that: OptionT[F, A])(implicit eq: Eq[F[Option[A]]]): Boolean =
    eq.eqv(value, that.value)

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *  scala> import scala.util.Right
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT(List(Some(2), None, Some(5)))
   *  scala> optionT.traverse[Either[Int, *], Int](x => Right[Int, Int](x))
   *  res0: Either[Int, OptionT[List, Int]] = Right(OptionT(List(Some(2), None, Some(5))))
   * }}}
   */
  def traverse[G[_], B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[OptionT[F, B]] =
    G.map(F.compose(Traverse[Option]).traverse(value)(f))(OptionT.apply)

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT(List(Some(2), None, Some(5)))
   *  scala> optionT.mapAccumulate(0)((s, a) => (s + a, s"$a!"))
   *  res0: (Int, OptionT[List, String]) = (7,OptionT(List(Some(2!), None, Some(5!))))
   * }}}
   */
  def mapAccumulate[S, B](init: S)(f: (S, A) => (S, B))(implicit traverseF: Traverse[F]): (S, OptionT[F, B]) = {
    val (snext, vnext) = traverseF.mapAccumulate(init, value)(Traverse[Option].mapAccumulate(_, _)(f))
    (snext, OptionT(vnext))
  }

  /**
   * Example:
   * {{{
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT(List(Some(2), None, Some(5)))
   *  scala> optionT.foldLeft(0)((acc, x) => acc + x)
   *  res0: Int = 7
   * }}}
   */
  def foldLeft[B](b: B)(f: (B, A) => B)(implicit F: Foldable[F]): B =
    F.compose(Foldable[Option]).foldLeft(value, b)(f)

  /**
   * Example:
   * {{{
   *  scala> import cats.Eval
   *  scala> import cats.data.OptionT
   *
   *  scala> val optionT: OptionT[List, Int] = OptionT(List(Some(2), None, Some(5)))
   *  scala> optionT.foldRight(Eval.One)((x, acc) => Eval.later(x * acc.value)).value
   *  res0: Int = 10
   * }}}
   */
  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B])(implicit F: Foldable[F]): Eval[B] =
    F.compose(Foldable[Option]).foldRight(value, lb)(f)

  /**
   * Transform this `OptionT[F, A]` into a `[[Nested]][F, Option, A]`.
   *
   * An example where `toNested` can be used, is to get the `Apply.ap` function with the
   * behavior from the composed `Apply` instances from `F` and `Option`, which is
   * inconsistent with the behavior of the `ap` from `Monad` of `OptionT`.
   *
   * {{{
   * scala> import cats.data.OptionT
   * scala> import cats.syntax.all._
   * scala> val ff: OptionT[List, Int => String] =
   *      |   OptionT(List(Option(_.toString), None))
   * scala> val fa: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
   * scala> ff.ap(fa)
   * res0: OptionT[List,String] = OptionT(List(Some(1), Some(2), None))
   * scala> OptionT(ff.toNested.ap(fa.toNested).value)
   * res1: OptionT[List,String] = OptionT(List(Some(1), Some(2), None, None))
   * }}}
   */
  def toNested: Nested[F, Option, A] = Nested(value)
}

object OptionT extends OptionTInstances {

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class PurePartiallyApplied[F[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[A](value: A)(implicit F: Applicative[F]): OptionT[F, A] =
      OptionT(F.pure(Some(value)))
  }

  /**
   * Creates a `OptionT[A]` from an `A`
   *
   * {{{
   * scala> OptionT.pure[List](2)
   * res0: OptionT[List, Int] = OptionT(List(Some(2)))
   * }}}
   */
  def pure[F[_]]: PurePartiallyApplied[F] = new PurePartiallyApplied[F]

  /**
   * An alias for pure
   *
   * {{{
   * scala> OptionT.some[List](2)
   * res0: OptionT[List, Int] = OptionT(List(Some(2)))
   * }}}
   */
  def some[F[_]]: PurePartiallyApplied[F] = pure

  def none[F[_], A](implicit F: Applicative[F]): OptionT[F, A] =
    OptionT(F.pure(None))

  /**
   * Transforms an `Option` into an `OptionT`, lifted into the specified `Applicative`.
   *
   * {{{
   * scala> val o: Option[Int] = Some(2)
   * scala> OptionT.fromOption[List](o)
   * res0: OptionT[List, Int] = OptionT(List(Some(2)))
   * }}}
   */
  def fromOption[F[_]]: FromOptionPartiallyApplied[F] = new FromOptionPartiallyApplied

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class FromOptionPartiallyApplied[F[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[A](value: Option[A])(implicit F: Applicative[F]): OptionT[F, A] =
      OptionT(F.pure(value))
  }

  /**
   * Lifts the `F[A]` Functor into an `OptionT[F, A]`.
   */
  def liftF[F[_], A](fa: F[A])(implicit F: Functor[F]): OptionT[F, A] = OptionT(F.map(fa)(Some(_)))

  /**
   * Same as [[liftF]], but expressed as a FunctionK for use with mapK
   * {{{
   * scala> import cats._, data._,  syntax.all._
   * scala> val a: EitherT[Eval, String, Int] = 1.pure[EitherT[Eval, String, *]]
   * scala> val b: EitherT[OptionT[Eval, *], String, Int] = a.mapK(OptionT.liftK)
   * scala> b.value.value.value
   * res0: Option[Either[String,Int]] = Some(Right(1))
   * }}}
   */
  def liftK[F[_]](implicit F: Functor[F]): F ~> OptionT[F, *] =
    new (F ~> OptionT[F, *]) { def apply[A](a: F[A]): OptionT[F, A] = OptionT.liftF(a) }

  /**
   * Creates a non-empty `OptionT[F, A]` from an `A` value if the given condition is `true`.
   * Otherwise, `none[F, A]` is returned. Analogous to `Option.when`.
   */
  def when[F[_], A](cond: Boolean)(a: => A)(implicit F: Applicative[F]): OptionT[F, A] =
    if (cond) OptionT.some[F](a) else OptionT.none[F, A]

  /**
   * Creates a non-empty `OptionT[F, A]` from an `F[A]` value if the given condition is `true`.
   * Otherwise, `none[F, A]` is returned. Analogous to `Option.when`.
   */
  def whenF[F[_], A](cond: Boolean)(fa: => F[A])(implicit F: Applicative[F]): OptionT[F, A] =
    if (cond) OptionT.liftF(fa) else OptionT.none[F, A]

  /**
   * Creates a non-empty `OptionT[F, A]` from an `F[A]` value if the given F-condition is considered `true`.
   * Otherwise, `none[F, A]` is returned. Analogous to `Option.when` but for effectful conditions.
   */
  def whenM[F[_], A](cond: F[Boolean])(fa: => F[A])(implicit F: Monad[F]): OptionT[F, A] = OptionT(
    F.ifM(cond)(ifTrue = F.map(fa)(Some(_)), ifFalse = F.pure(None))
  )

  /**
   * Same as `whenF`, but expressed as a FunctionK for use with mapK.
   */
  def whenK[F[_]](cond: Boolean)(implicit F: Applicative[F]): F ~> OptionT[F, *] =
    new (F ~> OptionT[F, *]) { def apply[A](a: F[A]): OptionT[F, A] = OptionT.whenF(cond)(a) }

  /**
   * Creates a non-empty `OptionT[F, A]` from an `A` if the given condition is `false`.
   * Otherwise, `none[F, A]` is returned. Analogous to `Option.unless`.
   */
  def unless[F[_], A](cond: Boolean)(a: => A)(implicit F: Applicative[F]): OptionT[F, A] =
    OptionT.when(!cond)(a)

  /**
   * Creates an non-empty `OptionT[F, A]` from an `F[A]` if the given condition is `false`.
   * Otherwise, `none[F, A]` is returned. Analogous to `Option.unless`.
   */
  def unlessF[F[_], A](cond: Boolean)(fa: => F[A])(implicit F: Applicative[F]): OptionT[F, A] =
    OptionT.whenF(!cond)(fa)

  /**
   * Creates a non-empty `OptionT[F, A]` from an `F[A]` value if the given F-condition is considered `false`.
   * Otherwise, `none[F, A]` is returned. Analogous to `Option.unless` but for effectful conditions.
   */
  def unlessM[F[_], A](cond: F[Boolean])(fa: => F[A])(implicit F: Monad[F]): OptionT[F, A] = OptionT(
    F.ifM(cond)(ifTrue = F.pure(None), ifFalse = F.map(fa)(Some(_)))
  )

  /**
   * Same as `unlessF`, but expressed as a FunctionK for use with mapK.
   */
  def unlessK[F[_]](cond: Boolean)(implicit F: Applicative[F]): F ~> OptionT[F, *] =
    new (F ~> OptionT[F, *]) { def apply[A](a: F[A]): OptionT[F, A] = OptionT.unlessF(cond)(a) }
}

sealed abstract private[data] class OptionTInstances extends OptionTInstances0 {
  // to maintain binary compatibility
  def catsDataMonadForOptionT[F[_]](implicit F0: Monad[F]): Monad[OptionT[F, *]] =
    new OptionTMonad[F] { implicit val F = F0 }

  implicit def catsDataTraverseForOptionT[F[_]](implicit F0: Traverse[F]): Traverse[OptionT[F, *]] =
    new OptionTTraverse[F] with OptionTFunctor[F] { implicit val F = F0 }

  implicit def catsDataOrderForOptionT[F[_], A](implicit F0: Order[F[Option[A]]]): Order[OptionT[F, A]] =
    new OptionTOrder[F, A] { implicit val F = F0 }

  implicit def catsDataMonoidForOptionT[F[_], A](implicit F0: Monoid[F[Option[A]]]): Monoid[OptionT[F, A]] =
    new OptionTMonoid[F, A] { implicit val F = F0 }

  implicit def catsDataShowForOptionT[F[_], A](implicit F: Show[F[Option[A]]]): Show[OptionT[F, A]] =
    Contravariant[Show].contramap(F)(_.value)

  implicit def catsDataDeferForOptionT[F[_]](implicit F: Defer[F]): Defer[OptionT[F, *]] =
    new Defer[OptionT[F, *]] {
      def defer[A](fa: => OptionT[F, A]): OptionT[F, A] =
        OptionT(F.defer(fa.value))
    }

  implicit def catsDataTraverseFilterForOptionT[F[_]](implicit F0: Traverse[F]): TraverseFilter[OptionT[F, *]] =
    new OptionTFunctorFilter[F] with TraverseFilter[OptionT[F, *]] {
      implicit def F: Functor[F] = F0

      val traverse: Traverse[OptionT[F, *]] = OptionT.catsDataTraverseForOptionT[F]

      def traverseFilter[G[_], A, B](
        fa: OptionT[F, A]
      )(f: A => G[Option[B]])(implicit G: Applicative[G]): G[OptionT[F, B]] =
        G.map(Traverse[F].traverse[G, Option[A], Option[B]](fa.value) { oa =>
          TraverseFilter[Option].traverseFilter(oa)(f)
        })(OptionT[F, B])

      override def filterA[G[_], A](
        fa: OptionT[F, A]
      )(f: A => G[Boolean])(implicit G: Applicative[G]): G[OptionT[F, A]] =
        G.map(Traverse[F].traverse(fa.value)(TraverseFilter[Option].filterA[G, A](_)(f)))(OptionT[F, A])

    }

  @deprecated("renamed to catsDataTraverseFilterForOptionT", "2.0.0")
  def catsDateTraverseFilterForOptionT[F[_]](implicit F0: Traverse[F]): TraverseFilter[OptionT[F, *]] =
    catsDataTraverseFilterForOptionT

  implicit def catsDataParallelForOptionT[M[_]](implicit
    P: Parallel[M]
  ): Parallel.Aux[OptionT[M, *], Nested[P.F, Option, *]] =
    new Parallel[OptionT[M, *]] {
      type F[x] = Nested[P.F, Option, x]

      implicit val monadM: Monad[M] = P.monad

      def applicative: Applicative[Nested[P.F, Option, *]] =
        cats.data.Nested.catsDataApplicativeForNested(P.applicative, cats.instances.option.catsStdInstancesForOption)

      def monad: Monad[OptionT[M, *]] = cats.data.OptionT.catsDataMonadErrorMonadForOptionT[M]

      def sequential: Nested[P.F, Option, *] ~> OptionT[M, *] =
        new (Nested[P.F, Option, *] ~> OptionT[M, *]) {
          def apply[A](nested: Nested[P.F, Option, A]): OptionT[M, A] = OptionT(P.sequential(nested.value))
        }

      def parallel: OptionT[M, *] ~> Nested[P.F, Option, *] =
        new (OptionT[M, *] ~> Nested[P.F, Option, *]) {
          def apply[A](optT: OptionT[M, A]): Nested[P.F, Option, A] = Nested(P.parallel(optT.value))
        }
    }
}

sealed abstract private[data] class OptionTInstances0 extends OptionTInstances1 {

  // the Dummy type is to make this one more specific than catsDataMonadErrorMonadForOptionT on 2.13.x
  // see https://github.com/typelevel/cats/pull/2335#issuecomment-408249775
  implicit def catsDataMonadErrorForOptionT[F[_], E](implicit
    F0: MonadError[F, E]
  ): MonadError[OptionT[F, *], E] { type Dummy } =
    new OptionTMonadError[F, E] {
      type Dummy
      implicit val F = F0
    }

  implicit def catsDataContravariantMonoidalForOptionT[F[_]](implicit
    F0: ContravariantMonoidal[F]
  ): ContravariantMonoidal[OptionT[F, *]] =
    new OptionTContravariantMonoidal[F] { implicit val F = F0 }

  implicit def catsDataMonoidKForOptionT[F[_]](implicit F0: Monad[F]): MonoidK[OptionT[F, *]] =
    new OptionTMonoidK[F] { implicit val F = F0 }

  implicit def catsDataSemigroupForOptionT[F[_], A](implicit F0: Semigroup[F[Option[A]]]): Semigroup[OptionT[F, A]] =
    new OptionTSemigroup[F, A] { implicit val F = F0 }

  implicit def catsDataPartialOrderForOptionT[F[_], A](implicit
    F0: PartialOrder[F[Option[A]]]
  ): PartialOrder[OptionT[F, A]] =
    new OptionTPartialOrder[F, A] { implicit val F = F0 }

  implicit def catsDateFunctorFilterForOptionT[F[_]](implicit F0: Functor[F]): FunctorFilter[OptionT[F, *]] =
    new OptionTFunctorFilter[F] { implicit val F = F0 }

  implicit def catsDataContravariantForOptionT[F[_]](implicit F0: Contravariant[F]): Contravariant[OptionT[F, *]] =
    new OptionTContravariant[F] { implicit val F = F0 }
}

sealed abstract private[data] class OptionTInstances1 extends OptionTInstances2 {
  implicit def catsDataSemigroupKForOptionT[F[_]](implicit F0: Monad[F]): SemigroupK[OptionT[F, *]] =
    new OptionTSemigroupK[F] { implicit val F = F0 }

  implicit def catsDataEqForOptionT[F[_], A](implicit F0: Eq[F[Option[A]]]): Eq[OptionT[F, A]] =
    new OptionTEq[F, A] { implicit val F = F0 }

  implicit def catsDataMonadErrorMonadForOptionT[F[_]](implicit F0: Monad[F]): MonadError[OptionT[F, *], Unit] =
    new OptionTMonadErrorMonad[F] { implicit val F = F0 }
}

sealed abstract private[data] class OptionTInstances2 extends OptionTInstances3 {
  implicit def catsDataFoldableForOptionT[F[_]](implicit F0: Foldable[F]): Foldable[OptionT[F, *]] =
    new OptionTFoldable[F] { implicit val F = F0 }

  implicit def catsDataInvariantForOptionT[F[_]](implicit F0: Invariant[F]): Invariant[OptionT[F, *]] =
    new OptionTInvariant[F] { implicit val F = F0 }
}

sealed abstract private[data] class OptionTInstances3 {
  implicit def catsDataFunctorForOptionT[F[_]](implicit F0: Functor[F]): Functor[OptionT[F, *]] =
    new OptionTFunctor[F] { implicit val F = F0 }
}

private[data] trait OptionTFunctor[F[_]] extends Functor[OptionT[F, *]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: OptionT[F, A])(f: A => B): OptionT[F, B] = fa.map(f)
}

sealed private[data] trait OptionTInvariant[F[_]] extends Invariant[OptionT[F, *]] {
  implicit def F: Invariant[F]

  override def imap[A, B](fa: OptionT[F, A])(f: A => B)(g: B => A): OptionT[F, B] =
    fa.imap(f)(g)
}

sealed private[data] trait OptionTContravariant[F[_]] extends Contravariant[OptionT[F, *]] {
  implicit def F: Contravariant[F]

  override def contramap[A, B](fa: OptionT[F, A])(f: B => A): OptionT[F, B] =
    fa.contramap(f)
}

private[data] trait OptionTMonad[F[_]] extends Monad[OptionT[F, *]] {
  implicit def F: Monad[F]

  def pure[A](a: A): OptionT[F, A] = OptionT.pure(a)

  def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] = fa.flatMap(f)

  override def map[A, B](fa: OptionT[F, A])(f: A => B): OptionT[F, B] = fa.map(f)

  def tailRecM[A, B](a: A)(f: A => OptionT[F, Either[A, B]]): OptionT[F, B] =
    OptionT(
      F.tailRecM(a)(a0 =>
        F.map(f(a0).value)(
          _.fold[Either[A, Option[B]]](Right(None))(_.map(b => Some(b): Option[B]))
        )
      )
    )
}

private[data] trait OptionTMonadErrorMonad[F[_]] extends MonadError[OptionT[F, *], Unit] with OptionTMonad[F] {
  implicit def F: Monad[F]

  override def raiseError[A](e: Unit): OptionT[F, A] = OptionT.none

  override def handleErrorWith[A](fa: OptionT[F, A])(f: Unit => OptionT[F, A]): OptionT[F, A] =
    OptionT(F.flatMap(fa.value) {
      case s @ Some(_) => F.pure(s)
      case None        => f(()).value
    })
}

private trait OptionTMonadError[F[_], E] extends MonadError[OptionT[F, *], E] with OptionTMonad[F] {
  override def F: MonadError[F, E]

  override def raiseError[A](e: E): OptionT[F, A] =
    OptionT(F.map(F.raiseError[A](e))(Some(_)))

  override def handleErrorWith[A](fa: OptionT[F, A])(f: E => OptionT[F, A]): OptionT[F, A] =
    OptionT(F.handleErrorWith(fa.value)(f(_).value))
}

private trait OptionTContravariantMonoidal[F[_]] extends ContravariantMonoidal[OptionT[F, *]] {
  def F: ContravariantMonoidal[F]

  override def unit: OptionT[F, Unit] = OptionT(F.trivial)

  override def contramap[A, B](fa: OptionT[F, A])(f: B => A): OptionT[F, B] =
    OptionT(F.contramap(fa.value)(_.map(f)))

  override def product[A, B](fa: OptionT[F, A], fb: OptionT[F, B]): OptionT[F, (A, B)] =
    OptionT(
      F.contramap(F.product(fa.value, fb.value))((t: Option[(A, B)]) =>
        t match {
          case Some((x, y)) => (Some(x), Some(y))
          case None         => (None, None)
        }
      )
    )
}

private[data] trait OptionTFoldable[F[_]] extends Foldable[OptionT[F, *]] {
  implicit def F: Foldable[F]

  def foldLeft[A, B](fa: OptionT[F, A], b: B)(f: (B, A) => B): B =
    fa.foldLeft(b)(f)

  def foldRight[A, B](fa: OptionT[F, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    fa.foldRight(lb)(f)
}

sealed private[data] trait OptionTTraverse[F[_]] extends Traverse[OptionT[F, *]] with OptionTFoldable[F] {
  implicit def F: Traverse[F]

  def traverse[G[_]: Applicative, A, B](fa: OptionT[F, A])(f: A => G[B]): G[OptionT[F, B]] =
    fa.traverse(f)

  override def mapAccumulate[S, A, B](init: S, fa: OptionT[F, A])(f: (S, A) => (S, B)): (S, OptionT[F, B]) =
    fa.mapAccumulate(init)(f)
}

private[data] trait OptionTSemigroup[F[_], A] extends Semigroup[OptionT[F, A]] {
  implicit val F: Semigroup[F[Option[A]]]

  def combine(x: OptionT[F, A], y: OptionT[F, A]): OptionT[F, A] =
    OptionT(F.combine(x.value, y.value))
}

private[data] trait OptionTMonoid[F[_], A] extends Monoid[OptionT[F, A]] with OptionTSemigroup[F, A] {
  implicit val F: Monoid[F[Option[A]]]

  def empty: OptionT[F, A] = OptionT(F.empty)
}

private[data] trait OptionTSemigroupK[F[_]] extends SemigroupK[OptionT[F, *]] {
  implicit def F: Monad[F]

  def combineK[A](x: OptionT[F, A], y: OptionT[F, A]): OptionT[F, A] = x.orElse(y)

  override def combineKEval[A](x: OptionT[F, A], y: Eval[OptionT[F, A]]): Eval[OptionT[F, A]] =
    Eval.now(OptionT(F.flatMap(x.value) {
      case oa @ Some(_) => F.pure(oa)
      case None         => y.value.value
    }))
}

private[data] trait OptionTMonoidK[F[_]] extends MonoidK[OptionT[F, *]] with OptionTSemigroupK[F] {
  def empty[A]: OptionT[F, A] = OptionT.none[F, A]
}

sealed private[data] trait OptionTEq[F[_], A] extends Eq[OptionT[F, A]] {
  implicit def F: Eq[F[Option[A]]]

  override def eqv(x: OptionT[F, A], y: OptionT[F, A]): Boolean = x === y
}

sealed private[data] trait OptionTPartialOrder[F[_], A] extends PartialOrder[OptionT[F, A]] with OptionTEq[F, A] {
  implicit override def F: PartialOrder[F[Option[A]]]

  override def partialCompare(x: OptionT[F, A], y: OptionT[F, A]): Double = x.partialCompare(y)
}

sealed private[data] trait OptionTFunctorFilter[F[_]] extends FunctorFilter[OptionT[F, *]] {
  implicit def F: Functor[F]

  def functor: Functor[OptionT[F, *]] = OptionT.catsDataFunctorForOptionT[F]

  def mapFilter[A, B](fa: OptionT[F, A])(f: (A) => Option[B]): OptionT[F, B] = fa.subflatMap(f)

  override def collect[A, B](fa: OptionT[F, A])(f: PartialFunction[A, B]): OptionT[F, B] = fa.subflatMap(f.lift)

  override def flattenOption[A](fa: OptionT[F, Option[A]]): OptionT[F, A] = fa.subflatMap(identity)

  override def filter[A](fa: OptionT[F, A])(f: (A) => Boolean): OptionT[F, A] = fa.filter(f)

  override def filterNot[A](fa: OptionT[F, A])(f: A => Boolean): OptionT[F, A] = fa.filterNot(f)
}

sealed private[data] trait OptionTOrder[F[_], A] extends Order[OptionT[F, A]] with OptionTPartialOrder[F, A] {
  implicit override def F: Order[F[Option[A]]]

  override def compare(x: OptionT[F, A], y: OptionT[F, A]): Int = x.compare(y)
}
