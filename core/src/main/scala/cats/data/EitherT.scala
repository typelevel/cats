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

import cats.Bifunctor
import cats.syntax.EitherUtil

/**
 * Transformer for `Either`, allowing the effect of an arbitrary type constructor `F` to be combined with the
 * fail-fast effect of `Either`.
 *
 * `EitherT[F, A, B]` wraps a value of type `F[Either[A, B]]`. An `F[C]` can be lifted in to `EitherT[F, A, C]` via `EitherT.right`,
 * and lifted in to a `EitherT[F, C, B]` via `EitherT.left`.
 */
final case class EitherT[F[_], A, B](value: F[Either[A, B]]) {

  /**
   * Transform this `EitherT[F, A, B]` into a `F[C]`.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   *
   * scala> val eitherT: EitherT[List, String, Int] = EitherT[List, String, Int](List(Left("456"), Right(123)))
   * scala> eitherT.fold(string => string.toInt, int => int)
   * res0: List[Int] = List(456, 123)
   * }}}
   */
  def fold[C](fa: A => C, fb: B => C)(implicit F: Functor[F]): F[C] = F.map(value)(_.fold(fa, fb))

  /**
   * Transform this `EitherT[F, A, B]` into a `F[C]`.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   *
   * scala> val eitherT: EitherT[List, String, Int] = EitherT[List, String, Int](List(Right(123),Left("abc")))
   * scala> eitherT.foldF(string => string.split("").toList, _ => List("123"))
   * res0: List[String] = List(123, a, b, c)
   * }}}
   */
  def foldF[C](fa: A => F[C], fb: B => F[C])(implicit F: FlatMap[F]): F[C] = F.flatMap(value)(_.fold(fa, fb))

  /**
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   *
   * scala> val eitherT: EitherT[List,String,Int] = EitherT[List, String,Int](List(Right(123),Left("abc")))
   * scala> eitherT.isLeft
   * res0: List[Boolean] = List(false, true)
   * }}}
   */
  def isLeft(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isLeft)

  /**
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   *
   * scala> val eitherT: EitherT[List,String,Int] = EitherT[List, String,Int](List(Right(123),Left("abc")))
   * scala> eitherT.isRight
   * res0: List[Boolean] = List(true, false)
   * }}}
   */
  def isRight(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isRight)

  /**
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   *
   * scala> val eitherT: EitherT[List,String,Int] = EitherT[List, String,Int](List(Right(123),Left("abc")))
   * scala> eitherT.swap
   * res0: EitherT[List,Int,String] = EitherT(List(Left(123), Right(abc)))
   * }}}
   */
  def swap(implicit F: Functor[F]): EitherT[F, B, A] = EitherT(F.map(value)(_.swap))

  /**
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   *
   * scala> val eitherT: EitherT[List,String,Int] = EitherT[List, String,Int](List(Right(123),Left("abc")))
   * scala> eitherT.getOrElse(456)
   * res0: List[Int] = List(123, 456)
   * }}}
   */
  def getOrElse[BB >: B](default: => BB)(implicit F: Functor[F]): F[BB] = F.map(value)(_.getOrElse(default))

  /**
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   *
   * scala> val eitherT: EitherT[List,String,Int] = EitherT[List, String,Int](List(Right(123),Left("abc")))
   * scala> eitherT.getOrElseF(List(456))
   * res0: List[Int] = List(123, 456)
   * }}}
   */
  def getOrElseF[BB >: B](default: => F[BB])(implicit F: Monad[F]): F[BB] =
    F.flatMap(value) {
      case Left(_)  => default
      case Right(b) => F.pure(b)
    }

  /***
   * 
   * Like [[getOrElseF]] but accept an error `E` and raise it when the inner `Either` is `Left`
   *    
   * Equivalent to `getOrElseF(F.raiseError(e)))`
   *    
   * Example:
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.syntax.all._
   * scala> import scala.util.{Success, Failure, Try}
  
   * scala> val eitherT: EitherT[Try,String,Int] = EitherT[Try,String,Int](Success(Left("abc")))
   * scala> eitherT.getOrRaise(new RuntimeException("ERROR!"))
   * res0: Try[Int] = Failure(java.lang.RuntimeException: ERROR!)
   * }}}
   */
  def getOrRaise[E](e: => E)(implicit F: MonadError[F, ? >: E]): F[B] =
    getOrElseF(F.raiseError(e))

  /**
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   *
   * scala> val e1: EitherT[Option,String,Int] = EitherT[Option, String,Int](Some(Right(123)))
   * scala> e1.orElse(EitherT[Option,Boolean,Int](Some(Right(456))))
   * res0: EitherT[Option, Boolean, Int] = EitherT(Some(Right(123)))
   *
   * scala> val e2: EitherT[Option,String,Int] = EitherT[Option, String,Int](Some(Left("abc")))
   * scala> e2.orElse(EitherT[Option,Boolean,Int](Some(Left(true))))
   * res1: EitherT[Option, Boolean, Int] = EitherT(Some(Left(true)))
   * }}}
   */
  def orElse[C, BB >: B](default: => EitherT[F, C, BB])(implicit F: Monad[F]): EitherT[F, C, BB] =
    EitherT(F.flatMap(value) {
      case Left(_)      => default.value
      case r @ Right(_) => F.pure(EitherUtil.leftCast(r))
    })

  /**
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   *
   * scala> val eitherT: EitherT[List,String,Int] =
   *      |   EitherT[List, String,Int](List(Right(123),Left("abc")))
   * scala> val pf: PartialFunction[String, Int] = {case "abc" => 456}
   * scala> eitherT.recover(pf)
   * res0: EitherT[List, String, Int] = EitherT(List(Right(123), Right(456)))
   * }}}
   */
  def recover(pf: PartialFunction[A, B])(implicit F: Functor[F]): EitherT[F, A, B] =
    EitherT(
      F.map(value) { eab =>
        eab match {
          case Left(a) if pf.isDefinedAt(a) => Right(pf(a))
          case _                            => eab
        }
      }
    )

  /**
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   *
   * scala> val eitherT: EitherT[List,String,Int] =
   *      |   EitherT[List, String,Int](List(Right(123),Left("abc")))
   * scala> val pf: PartialFunction[String, EitherT[List, String, Int]] =
   *      |   {case "abc" => EitherT[List, String, Int](List(Right(456)))}
   * scala> eitherT.recoverWith(pf)
   * res0: EitherT[List, String, Int] = EitherT(List(Right(123), Right(456)))
   * }}}
   */
  def recoverWith(pf: PartialFunction[A, EitherT[F, A, B]])(implicit F: Monad[F]): EitherT[F, A, B] =
    EitherT(F.flatMap(value) {
      case Left(a) if pf.isDefinedAt(a) => pf(a).value
      case other                        => F.pure(other)
    })

  /**
   * Inverse of `MonadError#attemptT`
   * Given MonadError[F, E :> A] transforms Either[F, A, B] to F[B]
   * If the value was B, F[B] is successful
   * If the value was A, F[B] is failed with E
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   *
   * scala> val e1: EitherT[Option, Unit, Int] = EitherT[Option, Unit, Int](Some(Right(123)))
   * scala> e1.rethrowT
   * res0: Option[Int] = Some(123)
   *
   * scala> val e2: EitherT[Option, Unit, Int] = EitherT[Option, Unit, Int](Some(Left(())))
   * scala> e2.rethrowT
   * res1: Option[Int] = None
   *
   * scala> import scala.util.Try
   * scala> import java.lang.Exception
   *
   * scala> val e3: EitherT[Try, Throwable, String] = EitherT[Try, Throwable, String](Try(Right("happy cats")))
   * scala> e3.rethrowT
   * res2: util.Try[String] = Success(happy cats)
   *
   * scala> val e4: EitherT[Try, Throwable, String] = EitherT[Try, Throwable, String](Try(Left(new Exception("sad cats"))))
   * scala> e4.rethrowT
   * res3: util.Try[String] = Failure(java.lang.Exception: sad cats)
   * }}}
   */
  def rethrowT(implicit F: MonadError[F, ? >: A]): F[B] =
    F.rethrow(value)

  /**
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   *
   * scala> val eitherT: EitherT[List, String, Int] = EitherT[List, String, Int](List(Right(123), Left("abc")))
   * scala> eitherT.valueOr(_.length)
   * res0: List[Int] = List(123, 3)
   * }}}
   */
  def valueOr[BB >: B](f: A => BB)(implicit F: Functor[F]): F[BB] = fold(f, identity)

  /**
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   *
   * scala> val eitherT: EitherT[List, String, Int] = EitherT[List, String, Int](List(Right(123), Left("abc")))
   * scala> eitherT.valueOrF(string => List(string.length))
   * res0: List[Int] = List(123, 3)
   * }}}
   */
  def valueOrF[BB >: B](f: A => F[BB])(implicit F: Monad[F]): F[BB] =
    F.flatMap(value) {
      case Left(a)  => f(a)
      case Right(b) => F.pure(b)
    }

  /**
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   *
   * scala> val eitherT: EitherT[List, String, Int] = EitherT[List, String, Int](List(Right(123), Left("abc")))
   * scala> eitherT.forall(_ > 100)
   * res0: List[Boolean] = List(true, true)
   * }}}
   */
  def forall(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.forall(f))

  /**
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   *
   * scala> val eitherT: EitherT[List, String, Int] = EitherT[List, String, Int](List(Right(123), Left("abc")))
   * scala> eitherT.exists(_ > 100)
   * res0: List[Boolean] = List(true, false)
   * }}}
   */
  def exists(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.exists(f))

  /**
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   *
   * scala> val e1: EitherT[List, String, Int] = EitherT[List, String, Int](List(Right(123), Left("abc")))
   * scala> e1.ensure("error")(_ > 150)
   * res0: EitherT[List, String, Int] = EitherT(List(Left(error), Left(abc)))
   *
   * scala> val e2: EitherT[List, String, Int] = EitherT[List, String, Int](List(Right(123), Left("abc")))
   * scala> e2.ensure("error")(_ > 100)
   * res1: EitherT[List, String, Int] = EitherT(List(Right(123), Left(abc)))
   * }}}
   */
  def ensure[AA >: A](onFailure: => AA)(f: B => Boolean)(implicit F: Functor[F]): EitherT[F, AA, B] =
    EitherT(
      F.map(value) { eab =>
        eab match {
          case Left(_)  => eab
          case Right(b) => if (f(b)) eab else Left(onFailure)
        }
      }
    )

  /**
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   *
   * scala> val e1: EitherT[List, String, Int] = EitherT[List, String, Int](List(Right(123), Left("abc")))
   * scala> e1.ensureOr(_ => "error")(_ > 100)
   * res0: EitherT[List, String, Int] = EitherT(List(Right(123), Left(abc)))
   *
   * scala> val e2: EitherT[List, String, Int] = EitherT[List, String, Int](List(Right(123), Left("abc")))
   * scala> e2.ensureOr(_ => "error")(_ > 150)
   * res1: EitherT[List, String, Int] = EitherT(List(Left(error), Left(abc)))
   * }}}
   */
  def ensureOr[AA >: A](onFailure: B => AA)(f: B => Boolean)(implicit F: Functor[F]): EitherT[F, AA, B] =
    EitherT(
      F.map(value) { eab =>
        eab match {
          case Left(_)  => eab
          case Right(b) => if (f(b)) eab else Left(onFailure(b))
        }
      }
    )

  /**
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   *
   * scala> val eitherT: EitherT[List, String, Int] = EitherT[List, String, Int](List(Right(123), Left("abc")))
   * scala> eitherT.toOption
   * res0: OptionT[List, Int] = OptionT(List(Some(123), None))
   * }}}
   */
  def toOption(implicit F: Functor[F]): OptionT[F, B] = OptionT(F.map(value)(_.toOption))

  /**
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   *
   * scala> val eitherT: EitherT[List, String, Int] = EitherT[List, String, Int](List(Right(123), Left("abc")))
   * scala> eitherT.to[Option]
   * res0: List[Option[Int]] = List(Some(123), None)
   * }}}
   */
  def to[G[_]](implicit F: Functor[F], G: Alternative[G]): F[G[B]] =
    F.map(value) {
      case Right(b) => G.pure(b)
      case Left(_)  => G.empty
    }

  /**
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   *
   * scala> val eitherT: EitherT[List, String, Int] =
   *      |   EitherT[List, String, Int](List(Right(123), Left("abc")))
   * scala> eitherT.collectRight
   * res0: List[Int] = List(123)
   * }}}
   */
  def collectRight(implicit FA: Alternative[F], FM: Monad[F]): F[B] =
    FM.flatMap(value) {
      case Right(b) => FA.pure(b)
      case Left(_)  => FA.empty
    }

  /**
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   *
   * scala> val eitherT: EitherT[List, String, Int] =
   *      |   EitherT[List, String, Int](List(Right(123), Left("abc")))
   * scala> eitherT.bimap(string => string.length, int => int % 100)
   * res0: EitherT[List, Int, Int] = EitherT(List(Right(23), Left(3)))
   * }}}
   */
  def bimap[C, D](fa: A => C, fb: B => D)(implicit F: Functor[F]): EitherT[F, C, D] =
    EitherT(
      F.map(value) {
        case Right(b) => Right(fb(b))
        case Left(a)  => Left(fa(a))
      }
    )

  def bitraverse[G[_], C, D](f: A => G[C], g: B => G[D])(implicit
    traverseF: Traverse[F],
    applicativeG: Applicative[G]
  ): G[EitherT[F, C, D]] =
    applicativeG.map(traverseF.traverse(value)(axb => Bitraverse[Either].bitraverse(axb)(f, g)))(EitherT.apply)

  def biflatMap[C, D](fa: A => EitherT[F, C, D], fb: B => EitherT[F, C, D])(implicit F: FlatMap[F]): EitherT[F, C, D] =
    EitherT(F.flatMap(value) {
      case Left(a)  => fa(a).value
      case Right(a) => fb(a).value
    })

  def applyAlt[D](ff: EitherT[F, A, B => D])(implicit F: Apply[F]): EitherT[F, A, D] =
    EitherT[F, A, D](F.map2(this.value, ff.value)((xb, xbd) => Apply[Either[A, *]].ap(xbd)(xb)))

  def flatMap[AA >: A, D](f: B => EitherT[F, AA, D])(implicit F: Monad[F]): EitherT[F, AA, D] =
    EitherT(F.flatMap(value) {
      case l @ Left(_) => F.pure(EitherUtil.rightCast(l))
      case Right(b)    => f(b).value
    })

  def flatMapF[AA >: A, D](f: B => F[Either[AA, D]])(implicit F: Monad[F]): EitherT[F, AA, D] =
    flatMap(b => EitherT(f(b)))

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

  def leftFlatMap[BB >: B, C](f: A => EitherT[F, C, BB])(implicit F: Monad[F]): EitherT[F, C, BB] =
    EitherT(F.flatMap(value) {
      case Left(a)      => f(a).value
      case r @ Right(_) => F.pure(EitherUtil.leftCast(r))
    })

  def leftSemiflatMap[D](f: A => F[D])(implicit F: Monad[F]): EitherT[F, D, B] =
    EitherT(F.flatMap(value) {
      case Left(a) =>
        F.map(f(a)) { d =>
          Left(d)
        }
      case r @ Right(_) => F.pure(EitherUtil.leftCast(r))
    })

  /**
   * Combine `leftSemiflatMap` and `semiflatMap` together.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> import cats.data.EitherT
   *
   * scala> val eitherT: EitherT[List, String, Int] = EitherT[List, String, Int](List(Left("abc"), Right(123)))
   * scala> eitherT.biSemiflatMap(string => List(string.length), int => List(int.toFloat))
   * res0: cats.data.EitherT[List,Int,Float] = EitherT(List(Left(3), Right(123.0)))
   * }}}
   */
  def biSemiflatMap[C, D](fa: A => F[C], fb: B => F[D])(implicit F: Monad[F]): EitherT[F, C, D] =
    EitherT(F.flatMap(value) {
      case Left(a) =>
        F.map(fa(a)) { c =>
          Left(c)
        }
      case Right(b) =>
        F.map(fb(b)) { d =>
          Right(d)
        }
    })

  def semiflatTap[C](f: B => F[C])(implicit F: Monad[F]): EitherT[F, A, B] =
    semiflatMap(b => F.as(f(b), b))

  def leftSemiflatTap[C](f: A => F[C])(implicit F: Monad[F]): EitherT[F, A, B] =
    leftSemiflatMap(a => F.as(f(a), a))

  def biSemiflatTap[C, D](fa: A => F[C], fb: B => F[D])(implicit F: FlatMap[F]): EitherT[F, A, B] =
    EitherT(F.flatMap(value) {
      case l @ Left(a) =>
        F.as(fa(a), l)
      case r @ Right(b) =>
        F.as(fb(b), r)
    })

  def compare(that: EitherT[F, A, B])(implicit o: Order[F[Either[A, B]]]): Int =
    o.compare(value, that.value)

  def partialCompare(that: EitherT[F, A, B])(implicit p: PartialOrder[F[Either[A, B]]]): Double =
    p.partialCompare(value, that.value)

  def ===(that: EitherT[F, A, B])(implicit eq: Eq[F[Either[A, B]]]): Boolean =
    eq.eqv(value, that.value)

  def traverse[G[_], D](
    f: B => G[D]
  )(implicit traverseF: Traverse[F], applicativeG: Applicative[G]): G[EitherT[F, A, D]] =
    applicativeG.map(traverseF.traverse(value)(axb => Traverse[Either[A, *]].traverse(axb)(f)))(EitherT.apply)

  def mapAccumulate[S, C](init: S)(f: (S, B) => (S, C))(implicit traverseF: Traverse[F]): (S, EitherT[F, A, C]) = {
    val (snext, vnext) = traverseF.mapAccumulate(init, value)(Traverse[Either[A, *]].mapAccumulate[S, B, C](_, _)(f))
    (snext, EitherT(vnext))
  }

  def foldLeft[C](c: C)(f: (C, B) => C)(implicit F: Foldable[F]): C =
    F.foldLeft(value, c) {
      case (c, Right(b)) => f(c, b)
      case (c, Left(_))  => c
    }

  def foldRight[C](lc: Eval[C])(f: (B, Eval[C]) => Eval[C])(implicit F: Foldable[F]): Eval[C] =
    F.foldRight(value, lc) {
      case (Right(b), lc) => f(b, lc)
      case (Left(_), lc)  => lc
    }

  def merge[AA >: A](implicit ev: B <:< AA, F: Functor[F]): F[AA] = F.map(value)(_.fold(identity, ev.apply))

  /**
   * Similar to `Either#combine` but mapped over an `F` context.
   *
   * Examples:
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.syntax.all._
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
    EitherT(
      F.map2(this.value, that.value) {
        case (Right(b1), Right(b2)) => Right(B.combine(b1, b2))
        case (left @ Left(_), _)    => left
        case (_, left @ Left(_))    => left
      }
    )

  def toValidated(implicit F: Functor[F]): F[Validated[A, B]] =
    F.map(value)(Validated.fromEither)

  def toValidatedNel(implicit F: Functor[F]): F[ValidatedNel[A, B]] =
    F.map(value) {
      case Right(b) => Validated.valid(b)
      case Left(a)  => Validated.invalidNel(a)
    }

  def toValidatedNec(implicit F: Functor[F]): F[ValidatedNec[A, B]] =
    F.map(value) {
      case Right(b) => Validated.valid(b)
      case Left(a)  => Validated.invalidNec(a)
    }

  /**
   * Run this value as a `[[Validated]]` against the function and convert it back to an `[[EitherT]]`.
   *
   * The [[Applicative]] instance for `EitherT` "fails fast" - it is often useful to "momentarily" have
   * it accumulate errors instead, which is what the `[[Validated]]` data type gives us.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> type Error = String
   * scala> val v1: Validated[NonEmptyList[Error], Int] = Validated.invalidNel("error 1")
   * scala> val v2: Validated[NonEmptyList[Error], Int] = Validated.invalidNel("error 2")
   * scala> val eithert: EitherT[Option, Error, Int] = EitherT.leftT[Option, Int]("error 3")
   * scala> eithert.withValidated { v3 => (v1, v2, v3.toValidatedNel).mapN { case (i, j, k) => i + j + k } }
   * res0: EitherT[Option, NonEmptyList[Error], Int] = EitherT(Some(Left(NonEmptyList(error 1, error 2, error 3))))
   * }}}
   */
  def withValidated[C, D](f: Validated[A, B] => Validated[C, D])(implicit F: Functor[F]): EitherT[F, C, D] =
    EitherT(F.map(value)(either => f(Validated.fromEither(either)).toEither))

  def show(implicit show: Show[F[Either[A, B]]]): String = show.show(value)

  /**
   * Transform this `EitherT[F, A, B]` into a `[[Nested]][F, Either[A, *], B]`.
   *
   * An example where `toNested` can be used, is to get the `Apply.ap` function with the
   * behavior from the composed `Apply` instances from `F` and `Either[A, *]`, which is
   * inconsistent with the behavior of the `ap` from `Monad` of `EitherT`.
   *
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.syntax.all._
   * scala> val ff: EitherT[List, String, Int => String] =
   *      |   EitherT(List(Either.right(_.toString), Either.left("error")))
   * scala> val fa: EitherT[List, String, Int] =
   *      |   EitherT(List(Either.right(1), Either.right(2)))
   * scala> ff.ap(fa)
   * res0: EitherT[List,String,String] = EitherT(List(Right(1), Right(2), Left(error)))
   * scala> EitherT((ff.toNested).ap(fa.toNested).value)
   * res1: EitherT[List,String,String] = EitherT(List(Right(1), Right(2), Left(error), Left(error)))
   * }}}
   */
  def toNested: Nested[F, Either[A, *], B] = Nested[F, Either[A, *], B](value)

  /**
   * Transform this `EitherT[F, A, B]` into a `[[Nested]][F, Validated[A, *], B]`.
   *
   * Example:
   * {{{
   * scala> import cats.data.{EitherT, Validated}
   * scala> import cats.syntax.all._
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
  def toNestedValidated(implicit F: Functor[F]): Nested[F, Validated[A, *], B] =
    Nested[F, Validated[A, *], B](F.map(value)(Validated.fromEither))

  /**
   * Transform this `EitherT[F, A, B]` into a `[[Nested]][F, ValidatedNel[A, *], B]`.
   */
  def toNestedValidatedNel(implicit F: Functor[F]): Nested[F, ValidatedNel[A, *], B] =
    Nested[F, ValidatedNel[A, *], B](
      F.map(value) {
        case Right(b) => Validated.valid(b)
        case Left(a)  => Validated.invalidNel(a)
      }
    )

  /**
   * Transform this `EitherT[F, A, B]` into a `[[Nested]][F, ValidatedNec[A, *], B]`.
   */
  def toNestedValidatedNec(implicit F: Functor[F]): Nested[F, ValidatedNec[A, *], B] =
    Nested[F, ValidatedNec[A, *], B](
      F.map(value) {
        case Right(b) => Validated.valid(b)
        case Left(a)  => Validated.invalidNec(a)
      }
    )

  /** Convert this `EitherT[F, A, B]` into an `IorT[F, A, B]`.
   */
  def toIor(implicit F: Functor[F]): IorT[F, A, B] =
    IorT.fromEitherF(value)
}

object EitherT extends EitherTInstances {

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class LeftPartiallyApplied[B](private val dummy: Boolean = true) extends AnyVal {
    def apply[F[_], A](fa: F[A])(implicit F: Functor[F]): EitherT[F, A, B] = EitherT(F.map(fa)(Left(_)))
  }

  /**
   * Creates a left version of `EitherT[F, A, B]` from a `F[A]`
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.syntax.all._
   * scala> EitherT.left[Int](Option("err"))
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Left(err)))
   * }}}
   */
  final def left[B]: LeftPartiallyApplied[B] = new LeftPartiallyApplied[B]

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class LeftTPartiallyApplied[F[_], B](private val dummy: Boolean = true) extends AnyVal {
    def apply[A](a: A)(implicit F: Applicative[F]): EitherT[F, A, B] = EitherT(F.pure(Left(a)))
  }

  /**
   * Creates a left version of `EitherT[F, A, B]` from a `A`
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.syntax.all._
   * scala> EitherT.leftT[Option, Int]("err")
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Left(err)))
   * }}}
   */
  final def leftT[F[_], B]: LeftTPartiallyApplied[F, B] = new LeftTPartiallyApplied[F, B]

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class RightPartiallyApplied[A](private val dummy: Boolean = true) extends AnyVal {
    def apply[F[_], B](fb: F[B])(implicit F: Functor[F]): EitherT[F, A, B] = EitherT(F.map(fb)(Right(_)))
  }

  /**
   * Creates a right version of `EitherT[F, A, B]` from a `F[B]`
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.syntax.all._
   * scala> EitherT.right[String](Option(3))
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Right(3)))
   * }}}
   */
  final def right[A]: RightPartiallyApplied[A] = new RightPartiallyApplied[A]

  /**
   * Uses the [[http://typelevel.org/cats/guidelines.html#partially-applied-type-params Partially Applied Type Params technique]] for ergonomics.
   */
  final private[data] class PurePartiallyApplied[F[_], A](private val dummy: Boolean = true) extends AnyVal {
    def apply[B](b: B)(implicit F: Applicative[F]): EitherT[F, A, B] = EitherT(F.pure(Right(b)))
  }

  /**
   * Creates a new `EitherT[F, A, B]` from a `B`
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.syntax.all._
   * scala> EitherT.pure[Option, String](3)
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Right(3)))
   * }}}
   */
  final def pure[F[_], A]: PurePartiallyApplied[F, A] = new PurePartiallyApplied[F, A]

  /**
   * Alias for [[pure]]
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.syntax.all._
   * scala> EitherT.rightT[Option, String](3)
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Right(3)))
   * }}}
   */
  final def rightT[F[_], A]: PurePartiallyApplied[F, A] = pure

  /**
   * Alias for [[right]]
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.syntax.all._
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
   * scala> val a: OptionT[Eval, Int] = 1.pure[OptionT[Eval, *]]
   * scala> val b: OptionT[EitherT[Eval, String, *], Int] = a.mapK(EitherT.liftK)
   * scala> b.value.value.value
   * res0: Either[String,Option[Int]] = Right(Some(1))
   * }}}
   */
  final def liftK[F[_], A](implicit F: Functor[F]): F ~> EitherT[F, A, *] =
    new (F ~> EitherT[F, A, *]) { def apply[B](fb: F[B]): EitherT[F, A, B] = right(fb) }

  /**
   * Lifts an effect into EitherT, catching all errors from the effect and lifting them into EitherT's error channel.
   *
   * {{{
   * scala> import cats._, data._, implicits._
   * scala> val a: Option[Int] = None
   * scala> val b: EitherT[Option, Unit, Int] = EitherT.liftAttemptK[Option, Unit].apply(a)
   * scala> b.value
   * res0: Option[Either[Unit, Int]] = Some(Left(()))
   *
   * scala> val a2: Option[Int] = Some(42)
   * scala> val b2: EitherT[Option, Unit, Int] = EitherT.liftAttemptK[Option, Unit].apply(a2)
   * scala> b2.value
   * res1: Option[Either[Unit, Int]] = Some(Right(42))
   * }}}
   */
  final def liftAttemptK[F[_], E](implicit F: ApplicativeError[F, E]): F ~> EitherT[F, E, *] =
    new (F ~> EitherT[F, E, *]) {
      def apply[A](fa: F[A]): EitherT[F, E, A] = EitherT(F.attempt(fa))
    }

  @deprecated("Use EitherT.liftF.", "1.0.0-RC1")
  final def liftT[F[_], A, B](fb: F[B])(implicit F: Functor[F]): EitherT[F, A, B] = right(fb)

  /**
   * Transforms an `Either` into an `EitherT`, lifted into the specified `Applicative`.
   *
   * Note: The return type is a FromEitherPartiallyApplied[F], which has an apply method
   * on it, allowing you to call fromEither like this:
   * {{{
   * scala> import cats.syntax.all._
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
  final private[data] class FromEitherPartiallyApplied[F[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](either: Either[E, A])(implicit F: Applicative[F]): EitherT[F, E, A] =
      EitherT(F.pure(either))
  }

  /**
   * Transforms an `Option` into an `EitherT`, lifted into the specified `Applicative` and using
   *  the second argument if the `Option` is a `None`.
   * {{{
   * scala> import cats.syntax.all._
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
  final private[data] class FromOptionPartiallyApplied[F[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](opt: Option[A], ifNone: => E)(implicit F: Applicative[F]): EitherT[F, E, A] =
      EitherT(
        F.pure(
          opt match {
            case Some(a) => Right(a)
            case None    => Left(ifNone)
          }
        )
      )
  }

  /**
   * Transforms an `F[Option]` into an `EitherT`, using the second argument if the `Option` is a `None`.
   * {{{
   * scala> import cats.syntax.all._
   * scala> val o: Option[Int] = None
   * scala> EitherT.fromOptionF(List(o), "Answer not known.")
   * res0: EitherT[List, String, Int]  = EitherT(List(Left(Answer not known.)))
   * scala> EitherT.fromOptionF(List(Option(42)), "Answer not known.")
   * res1: EitherT[List, String, Int] = EitherT(List(Right(42)))
   * }}}
   */
  final def fromOptionF[F[_], E, A](fopt: F[Option[A]], ifNone: => E)(implicit F: Functor[F]): EitherT[F, E, A] =
    EitherT(
      F.map(fopt) {
        case Some(a) => Right(a)
        case None    => Left(ifNone)
      }
    )

  /**
   * Similar to `fromOptionF` but the left is carried from monadic `F[_]` context when the option is `None`
   */
  final def fromOptionM[F[_], E, A](fopt: F[Option[A]], ifNone: => F[E])(implicit F: Monad[F]): EitherT[F, E, A] =
    EitherT(
      F.flatMap(fopt) {
        case Some(a) => F.pure(Right.apply[E, A](a))
        case None    => F.map(ifNone)(Left.apply[E, A])
      }
    )

  /**
   *  If the condition is satisfied, return the given `A` in `Right`
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
  final private[data] class CondPartiallyApplied[F[_]](private val dummy: Boolean = true) extends AnyVal {
    def apply[E, A](test: Boolean, right: => A, left: => E)(implicit F: Applicative[F]): EitherT[F, E, A] =
      EitherT(F.pure(Either.cond(test, right, left)))
  }
}

abstract private[data] class EitherTInstances extends EitherTInstances1 {

  implicit def catsDataOrderForEitherT[F[_], L, R](implicit F: Order[F[Either[L, R]]]): Order[EitherT[F, L, R]] =
    new EitherTOrder[F, L, R] {
      val F0: Order[F[Either[L, R]]] = F
    }

  implicit def catsDataShowForEitherT[F[_], L, R](implicit sh: Show[F[Either[L, R]]]): Show[EitherT[F, L, R]] =
    Contravariant[Show].contramap(sh)(_.value)

  implicit def catsDataBifunctorForEitherT[F[_]](implicit F: Functor[F]): Bifunctor[EitherT[F, *, *]] =
    new EitherTBifunctor[F] {
      val F0: Functor[F] = F
    }

  implicit def catsDataTraverseForEitherT[F[_], L](implicit FF: Traverse[F]): Traverse[EitherT[F, L, *]] =
    new EitherTTraverse[F, L] with EitherTFunctor[F, L] {
      val F0: Traverse[F] = FF
      val F: Functor[F] = FF
    }

  implicit def catsMonoidForEitherT[F[_], L, A](implicit F: Monoid[F[Either[L, A]]]): Monoid[EitherT[F, L, A]] =
    new EitherTMonoid[F, L, A] { implicit val F0 = F }

  implicit def catsDataDeferForEitherT[F[_], L](implicit F: Defer[F]): Defer[EitherT[F, L, *]] =
    new Defer[EitherT[F, L, *]] {
      def defer[A](fa: => EitherT[F, L, A]): EitherT[F, L, A] =
        EitherT(F.defer(fa.value))
    }

  @deprecated("This implicit provides inconsistent effect layering semantics; see #3776 for more discussion", "2.4.0")
  def catsDataParallelForEitherTWithParallelEffect[M[_], E: Semigroup](implicit
    P: Parallel[M]
  ): Parallel.Aux[EitherT[M, E, *], Nested[P.F, Validated[E, *], *]] =
    accumulatingParallel[M, E]

  /**
   * An alternative [[Parallel]] implementation which merges the semantics of
   * the outer Parallel (the F[_] effect) with the effects of the inner
   * one (the Either). The inner Parallel has the semantics of [[Validated]],
   * while the outer has the semantics of parallel ''evaluation'' (in most cases).
   * The default Parallel for [[EitherT]], when the nested F also has a Parallel,
   * is to strictly take the semantics of the nested F and to short-circuit any
   * lefts (often, errors) in a left-to-right fashion, mirroring the semantics of
   * [[Applicative]] on EitherT. This instance is different in that it will not
   * ''short-circuit'' but instead accumulate all lefts according to the supplied
   * [[Semigroup]], similar to Validated.
   *
   * {{{
   * implicit val p: Parallel[EitherT[IO, Chain[Error], *]] = EitherT.accumulatingParallel
   *
   * val a = EitherT(IO(Chain(error1).asLeft[Unit]))
   * val b = EitherT(IO(Chain(error2).asLeft[Unit]))
   *
   * (a, b).parTupled  // => EitherT(IO(Chain(error1, error2).asLeft[Unit]))
   * }}}
   */
  def accumulatingParallel[M[_], E: Semigroup](implicit
    P: Parallel[M]
  ): Parallel.Aux[EitherT[M, E, *], Nested[P.F, Validated[E, *], *]] =
    new Parallel[EitherT[M, E, *]] {
      type F[x] = Nested[P.F, Validated[E, *], x]

      implicit val monadM: Monad[M] = P.monad

      def applicative: Applicative[Nested[P.F, Validated[E, *], *]] =
        cats.data.Nested.catsDataApplicativeForNested(P.applicative, Validated.catsDataApplicativeErrorForValidated)

      def monad: Monad[EitherT[M, E, *]] = cats.data.EitherT.catsDataMonadErrorForEitherT

      def sequential: Nested[P.F, Validated[E, *], *] ~> EitherT[M, E, *] =
        new (Nested[P.F, Validated[E, *], *] ~> EitherT[M, E, *]) {
          def apply[A](nested: Nested[P.F, Validated[E, *], A]): EitherT[M, E, A] = {
            val mva = P.sequential(nested.value)
            EitherT(Functor[M].map(mva)(_.toEither))
          }
        }

      def parallel: EitherT[M, E, *] ~> Nested[P.F, Validated[E, *], *] =
        new (EitherT[M, E, *] ~> Nested[P.F, Validated[E, *], *]) {
          def apply[A](eitherT: EitherT[M, E, A]): Nested[P.F, Validated[E, *], A] = {
            val fea = P.parallel(eitherT.value)
            Nested(P.applicative.map(fea)(Validated.fromEither))
          }
        }
    }

  implicit def catsDataParallelForEitherTWithParallelEffect2[M[_], E](implicit
    P: Parallel[M]
  ): Parallel.Aux[EitherT[M, E, *], Nested[P.F, Either[E, *], *]] =
    new Parallel[EitherT[M, E, *]] {
      type F[x] = Nested[P.F, Either[E, *], x]

      implicit val monadM: Monad[M] = P.monad
      implicit val monadEither: Monad[Either[E, *]] = cats.instances.either.catsStdInstancesForEither

      def applicative: Applicative[Nested[P.F, Either[E, *], *]] =
        cats.data.Nested.catsDataApplicativeForNested(P.applicative, implicitly)

      def monad: Monad[EitherT[M, E, *]] = cats.data.EitherT.catsDataMonadErrorForEitherT

      def sequential: Nested[P.F, Either[E, *], *] ~> EitherT[M, E, *] =
        new (Nested[P.F, Either[E, *], *] ~> EitherT[M, E, *]) {
          def apply[A](nested: Nested[P.F, Either[E, *], A]): EitherT[M, E, A] = {
            val mva = P.sequential(nested.value)
            EitherT(Functor[M].map(mva)(x => x))
          }
        }

      def parallel: EitherT[M, E, *] ~> Nested[P.F, Either[E, *], *] =
        new (EitherT[M, E, *] ~> Nested[P.F, Either[E, *], *]) {
          def apply[A](eitherT: EitherT[M, E, A]): Nested[P.F, Either[E, *], A] = {
            val fea = P.parallel(eitherT.value)
            Nested(P.applicative.map(fea)(x => x))
          }
        }
    }
}

abstract private[data] class EitherTInstances1 extends EitherTInstances2 {

  implicit def catsSemigroupForEitherT[F[_], L, A](implicit
    F: Semigroup[F[Either[L, A]]]
  ): Semigroup[EitherT[F, L, A]] =
    new EitherTSemigroup[F, L, A] { implicit val F0 = F }

  implicit def catsDataFoldableForEitherT[F[_], L](implicit F: Foldable[F]): Foldable[EitherT[F, L, *]] =
    new EitherTFoldable[F, L] {
      val F0: Foldable[F] = F
    }

  implicit def catsDataPartialOrderForEitherT[F[_], L, R](implicit
    F: PartialOrder[F[Either[L, R]]]
  ): PartialOrder[EitherT[F, L, R]] =
    new EitherTPartialOrder[F, L, R] {
      val F0: PartialOrder[F[Either[L, R]]] = F
    }

  implicit def catsDataBitraverseForEitherT[F[_]](implicit F: Traverse[F]): Bitraverse[EitherT[F, *, *]] =
    new EitherTBitraverse[F] with EitherTBifunctor[F] {
      val F0: Traverse[F] = F
    }

  implicit def catsDataMonadErrorForEitherT[F[_], L](implicit F0: Monad[F]): MonadError[EitherT[F, L, *], L] =
    new EitherTMonadError[F, L] {
      implicit val F = F0
      override def ensure[A](fa: EitherT[F, L, A])(error: => L)(predicate: (A) => Boolean): EitherT[F, L, A] =
        fa.ensure(error)(predicate)(F)

      override def ensureOr[A](fa: EitherT[F, L, A])(error: (A) => L)(predicate: (A) => Boolean): EitherT[F, L, A] =
        fa.ensureOr(error)(predicate)(F)
    }

  implicit def catsDataParallelForEitherTWithSequentialEffect[M[_]: Monad, E: Semigroup]
    : Parallel.Aux[EitherT[M, E, *], Nested[M, Validated[E, *], *]] =
    new Parallel[EitherT[M, E, *]] {
      type F[x] = Nested[M, Validated[E, *], x]

      implicit val appValidated: Applicative[Validated[E, *]] = Validated.catsDataApplicativeErrorForValidated

      def applicative: Applicative[Nested[M, Validated[E, *], *]] =
        cats.data.Nested.catsDataApplicativeForNested[M, Validated[E, *]]

      def monad: Monad[EitherT[M, E, *]] = cats.data.EitherT.catsDataMonadErrorForEitherT

      def sequential: Nested[M, Validated[E, *], *] ~> EitherT[M, E, *] =
        new (Nested[M, Validated[E, *], *] ~> EitherT[M, E, *]) {
          def apply[A](nested: Nested[M, Validated[E, *], A]): EitherT[M, E, A] =
            EitherT(Monad[M].map(nested.value)(_.toEither))
        }

      def parallel: EitherT[M, E, *] ~> Nested[M, Validated[E, *], *] =
        new (EitherT[M, E, *] ~> Nested[M, Validated[E, *], *]) {
          def apply[A](eitherT: EitherT[M, E, A]): Nested[M, Validated[E, *], A] =
            Nested(Monad[M].map(eitherT.value)(Validated.fromEither))
        }
    }
}

abstract private[data] class EitherTInstances2 extends EitherTInstances3 {

  /**
   *  Monad error instance for recovering errors in F instead of
   *  the underlying Either.
   *
   * {{{
   * scala> import cats.data.EitherT
   * scala> import cats.MonadError
   * scala> import cats.instances.option._
   * scala> val noInt: Option[Either[String, Int]] = None
   * scala> val et = EitherT[Option, String, Int](noInt)
   * scala> val me = MonadError[EitherT[Option, String, *], Unit]
   * scala> me.recover(et) { case () => 1 }
   * res0: cats.data.EitherT[Option,String,Int] = EitherT(Some(Right(1)))
   * }}}
   */
  implicit def catsDataMonadErrorFForEitherT[F[_], E, L](implicit
    FE0: MonadError[F, E]
  ): MonadError[EitherT[F, L, *], E] =
    new EitherTMonadErrorF[F, E, L] { implicit val F = FE0 }

  implicit def catsDataSemigroupKForEitherT[F[_], L](implicit F0: Monad[F]): SemigroupK[EitherT[F, L, *]] =
    new EitherTSemigroupK[F, L] { implicit val F = F0 }

  implicit def catsDataEqForEitherT[F[_], L, R](implicit F: Eq[F[Either[L, R]]]): Eq[EitherT[F, L, R]] =
    new EitherTEq[F, L, R] {
      val F0: Eq[F[Either[L, R]]] = F
    }
}

abstract private[data] class EitherTInstances3 {
  implicit def catsDataFunctorForEitherT[F[_], L](implicit F0: Functor[F]): Functor[EitherT[F, L, *]] =
    new EitherTFunctor[F, L] { implicit val F = F0 }
}

private[data] trait EitherTSemigroup[F[_], L, A] extends Semigroup[EitherT[F, L, A]] {
  implicit val F0: Semigroup[F[Either[L, A]]]
  def combine(x: EitherT[F, L, A], y: EitherT[F, L, A]): EitherT[F, L, A] =
    EitherT(F0.combine(x.value, y.value))
}

private[data] trait EitherTMonoid[F[_], L, A] extends Monoid[EitherT[F, L, A]] with EitherTSemigroup[F, L, A] {
  implicit val F0: Monoid[F[Either[L, A]]]
  def empty: EitherT[F, L, A] = EitherT(F0.empty)
}

private[data] trait EitherTSemigroupK[F[_], L] extends SemigroupK[EitherT[F, L, *]] {
  implicit val F: Monad[F]
  def combineK[A](x: EitherT[F, L, A], y: EitherT[F, L, A]): EitherT[F, L, A] =
    EitherT(F.flatMap(x.value) {
      case r @ Right(_) => F.pure(r)
      case Left(_)      => y.value
    })

  override def combineKEval[A](x: EitherT[F, L, A], y: Eval[EitherT[F, L, A]]): Eval[EitherT[F, L, A]] =
    Eval.now(EitherT(F.flatMap(x.value) {
      case r @ Right(_) => F.pure(r: Either[L, A])
      case Left(_)      => y.value.value
    }))
}

private[data] trait EitherTFunctor[F[_], L] extends Functor[EitherT[F, L, *]] {
  implicit val F: Functor[F]
  override def map[A, B](fa: EitherT[F, L, A])(f: A => B): EitherT[F, L, B] = fa.map(f)
}

private[data] trait EitherTMonad[F[_], L] extends Monad[EitherT[F, L, *]] with EitherTFunctor[F, L] {
  implicit val F: Monad[F]
  def pure[A](a: A): EitherT[F, L, A] = EitherT.pure(a)

  def flatMap[A, B](fa: EitherT[F, L, A])(f: A => EitherT[F, L, B]): EitherT[F, L, B] = fa.flatMap(f)
  def tailRecM[A, B](a: A)(f: A => EitherT[F, L, Either[A, B]]): EitherT[F, L, B] =
    EitherT(
      F.tailRecM(a)(a0 =>
        F.map(f(a0).value) {
          case Left(l)         => Right(Left(l))
          case Right(Left(a1)) => Left(a1)
          case Right(Right(b)) => Right(Right(b))
        }
      )
    )
}

private[data] trait EitherTMonadErrorF[F[_], E, L] extends MonadError[EitherT[F, L, *], E] with EitherTMonad[F, L] {
  implicit val F: MonadError[F, E]

  def handleErrorWith[A](fea: EitherT[F, L, A])(f: E => EitherT[F, L, A]): EitherT[F, L, A] =
    EitherT(F.handleErrorWith(fea.value)(f(_).value))

  def raiseError[A](e: E): EitherT[F, L, A] = EitherT(F.raiseError(e))
}

private[data] trait EitherTMonadError[F[_], L] extends MonadError[EitherT[F, L, *], L] with EitherTMonad[F, L] {
  def handleErrorWith[A](fea: EitherT[F, L, A])(f: L => EitherT[F, L, A]): EitherT[F, L, A] =
    EitherT(F.flatMap(fea.value) {
      case Left(e)      => f(e).value
      case r @ Right(_) => F.pure(r)
    })
  override def handleError[A](fea: EitherT[F, L, A])(f: L => A): EitherT[F, L, A] =
    EitherT(F.flatMap(fea.value) {
      case Left(e)      => F.pure(Right(f(e)))
      case r @ Right(_) => F.pure(r)
    })
  def raiseError[A](e: L): EitherT[F, L, A] = EitherT.left(F.pure(e))
  override def attempt[A](fla: EitherT[F, L, A]): EitherT[F, L, Either[L, A]] = EitherT.right(fla.value)
  override def recover[A](fla: EitherT[F, L, A])(pf: PartialFunction[L, A]): EitherT[F, L, A] =
    fla.recover(pf)
  override def recoverWith[A](fla: EitherT[F, L, A])(pf: PartialFunction[L, EitherT[F, L, A]]): EitherT[F, L, A] =
    fla.recoverWith(pf)
}

sealed private[data] trait EitherTFoldable[F[_], L] extends Foldable[EitherT[F, L, *]] {
  implicit def F0: Foldable[F]

  def foldLeft[A, B](fa: EitherT[F, L, A], b: B)(f: (B, A) => B): B =
    fa.foldLeft(b)(f)

  def foldRight[A, B](fa: EitherT[F, L, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    fa.foldRight(lb)(f)
}

sealed private[data] trait EitherTTraverse[F[_], L] extends Traverse[EitherT[F, L, *]] with EitherTFoldable[F, L] {
  implicit override def F0: Traverse[F]

  override def traverse[G[_]: Applicative, A, B](fa: EitherT[F, L, A])(f: A => G[B]): G[EitherT[F, L, B]] =
    fa.traverse(f)

  override def mapAccumulate[S, A, B](init: S, fa: EitherT[F, L, A])(f: (S, A) => (S, B)): (S, EitherT[F, L, B]) =
    fa.mapAccumulate(init)(f)
}

sealed private[data] trait EitherTBifoldable[F[_]] extends Bifoldable[EitherT[F, *, *]] {
  implicit def F0: Foldable[F]

  def bifoldLeft[A, B, C](fab: EitherT[F, A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    F0.foldLeft(fab.value, c)((acc, axb) => Bifoldable[Either].bifoldLeft(axb, acc)(f, g))

  def bifoldRight[A, B, C](fab: EitherT[F, A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C],
                                                              g: (B, Eval[C]) => Eval[C]
  ): Eval[C] =
    F0.foldRight(fab.value, c)((axb, acc) => Bifoldable[Either].bifoldRight(axb, acc)(f, g))
}

sealed private[data] trait EitherTBitraverse[F[_]] extends Bitraverse[EitherT[F, *, *]] with EitherTBifoldable[F] {
  implicit override def F0: Traverse[F]

  override def bitraverse[G[_], A, B, C, D](
    fab: EitherT[F, A, B]
  )(f: A => G[C], g: B => G[D])(implicit G: Applicative[G]): G[EitherT[F, C, D]] =
    fab.bitraverse(f, g)
}

sealed private[data] trait EitherTBifunctor[F[_]] extends Bifunctor[EitherT[F, *, *]] {
  implicit def F0: Functor[F]

  override def bimap[A, B, C, D](fab: EitherT[F, A, B])(f: A => C, g: B => D): EitherT[F, C, D] = fab.bimap(f, g)
}

sealed private[data] trait EitherTEq[F[_], L, A] extends Eq[EitherT[F, L, A]] {
  implicit def F0: Eq[F[Either[L, A]]]

  override def eqv(x: EitherT[F, L, A], y: EitherT[F, L, A]): Boolean = x === y
}

sealed private[data] trait EitherTPartialOrder[F[_], L, A]
    extends PartialOrder[EitherT[F, L, A]]
    with EitherTEq[F, L, A] {
  implicit override def F0: PartialOrder[F[Either[L, A]]]

  override def partialCompare(x: EitherT[F, L, A], y: EitherT[F, L, A]): Double =
    x.partialCompare(y)
}

sealed private[data] trait EitherTOrder[F[_], L, A] extends Order[EitherT[F, L, A]] with EitherTPartialOrder[F, L, A] {
  implicit override def F0: Order[F[Either[L, A]]]

  override def compare(x: EitherT[F, L, A], y: EitherT[F, L, A]): Int = x.compare(y)
}
