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
import cats.data.Validated.{Invalid, Valid}

import scala.annotation.tailrec

/**
 * Represents a right-biased disjunction that is either an `A`, or a `B`, or both an `A` and a `B`.
 *
 * An instance of `A [[Ior]] B` is one of:
 *  - `[[Ior.Left Left]][A]`
 *  - `[[Ior.Right Right]][B]`
 *  - `[[Ior.Both Both]][A, B]`
 *
 * `A [[Ior]] B` is similar to `scala.util.Either[A, B]`, except that it can represent the simultaneous presence of
 * an `A` and a `B`. It is right-biased so methods such as `map` and `flatMap` operate on the
 * `B` value. Some methods, like `flatMap`, handle the presence of two [[Ior.Both Both]] values using a
 * `[[Semigroup]][A]`, while other methods, like [[toEither]], ignore the `A` value in a [[Ior.Both Both]].
 *
 * `A [[Ior]] B` is isomorphic to `Either[Either[A, B], (A, B)]`, but provides methods biased toward `B`
 * values, regardless of whether the `B` values appear in a [[Ior.Right Right]] or a [[Ior.Both Both]].
 * The isomorphic `scala.util.Either` form can be accessed via the [[unwrap]] method.
 */
sealed abstract class Ior[+A, +B] extends Product with Serializable {

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> val rightF: Int => String = _.show
   * scala> val bothF: (String,Int) => String = (a,b) => a.combine(b.show)
   *
   * scala> val ior1 = "abc".leftIor[Int]
   * scala> ior1.fold(identity, rightF, bothF)
   * res0: String = abc
   *
   * scala> val ior2 = 123.rightIor[String]
   * scala> ior2.fold(identity, rightF, bothF)
   * res1: String = 123
   *
   * scala> val ior3 = Ior.Both("abc", 123)
   * scala> ior3.fold(identity, rightF, bothF)
   * res2: String = abc123
   * }}}
   */
  final def fold[C](fa: A => C, fb: B => C, fab: (A, B) => C): C =
    this match {
      case Ior.Left(a)    => fa(a)
      case Ior.Right(b)   => fb(b)
      case Ior.Both(a, b) => fab(a, b)
    }

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> val ior1 = "abc".leftIor[Int]
   * scala> ior1.putLeft(true)
   * res0: Ior[Boolean, Int] = Left(true)
   *
   * scala> val ior2 = 123.rightIor[String]
   * scala> ior2.putLeft(false)
   * res1: Ior[Boolean, Int] = Both(false,123)
   *
   * scala> val ior3 = Ior.Both("abc",123)
   * scala> ior3.putLeft(true)
   * res2: Ior[Boolean, Int] = Both(true,123)
   * }}}
   */
  final def putLeft[C](left: C): C Ior B =
    fold(_ => Ior.left(left), Ior.both(left, _), (_, b) => Ior.both(left, b))

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> val ior1 = "abc".leftIor[Int]
   * scala> ior1.putRight(123L)
   * res0: Ior[String, Long] = Both(abc,123)
   *
   * scala> val ior2 = 123.rightIor[String]
   * scala> ior2.putRight(123L)
   * res1: Ior[String, Long] = Right(123)
   *
   * scala> val ior3 = Ior.Both("abc",123)
   * scala> ior3.putRight(123L)
   * res2: Ior[String, Long] = Both(abc,123)
   * }}}
   */
  final def putRight[C](right: C): A Ior C =
    fold(Ior.both(_, right), _ => Ior.right(right), (a, _) => Ior.both(a, right))

  /**
   * When a Left value is present in the Ior, combine it with the value specified.
   *
   * When the Left value is absent, set it to the value specified.
   *
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> val ior1 = "abc".leftIor[Int]
   * scala> ior1.addLeft("def")
   * res0: Ior[String, Int] = Left(abcdef)
   *
   * scala> val ior2 = 123.rightIor[String]
   * scala> ior2.addLeft("abc")
   * res1: cats.data.Ior[String,Int] = Both(abc,123)
   *
   * scala> val ior3 = Ior.Both("abc",123)
   * scala> ior3.addLeft("def")
   * res2: Ior[String, Int] = Both(abcdef,123)
   * }}}
   */
  final def addLeft[AA >: A](left: AA)(implicit AA: Semigroup[AA]): AA Ior B =
    fold(l => Ior.left(AA.combine(l, left)), Ior.both(left, _), (l, r) => Ior.both(AA.combine(l, left), r))

  /**
   * When a Right value is present in the Ior, combine it with the value specified.
   *
   * When the Right value is absent, set it to the value specified.
   *
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> val ior1 = "abc".leftIor[Int]
   * scala> ior1.addRight(123)
   * res0: Ior[String, Int] = Both(abc,123)
   *
   * scala> val ior2 = 123.rightIor[String]
   * scala> ior2.addRight(123)
   * res1: Ior[String, Int] = Right(246)
   *
   * scala> val ior3 = Ior.Both("abc",123)
   * scala> ior3.addRight(123)
   * res2: Ior[String, Int] = Both(abc,246)
   * }}}
   */
  final def addRight[BB >: B](right: BB)(implicit BB: Semigroup[BB]): A Ior BB =
    fold(Ior.both(_, right), r => Ior.right(BB.combine(r, right)), (l, r) => Ior.both(l, BB.combine(r, right)))

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].isLeft
   * res0: Boolean = true
   *
   * scala> 123.rightIor[String].isLeft
   * res1: Boolean = false
   *
   * scala> Ior.Both("abc", 123).isLeft
   * res2: Boolean = false
   * }}}
   */
  final def isLeft: Boolean =
    this match {
      case Ior.Left(_) => true
      case _           => false
    }

  final def isRight: Boolean =
    this match {
      case Ior.Right(_) => true
      case _            => false
    }

  final def isBoth: Boolean =
    this match {
      case Ior.Both(_, _) => true
      case _              => false
    }

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].left
   * res0: Option[String] = Some(abc)
   *
   * scala> 123.rightIor[String].left
   * res1: Option[String] = None
   *
   * scala> Ior.Both("abc", 123).left
   * res2: Option[String] = Some(abc)
   * }}}
   */
  final def left: Option[A] = fold(a => Some(a), _ => None, (a, _) => Some(a))

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].right
   * res0: Option[Int] = None
   *
   * scala> 123.rightIor[String].right
   * res1: Option[Int] = Some(123)
   *
   * scala> Ior.Both("abc", 123).right
   * res2: Option[Int] = Some(123)
   * }}}
   */
  final def right: Option[B] =
    this match {
      case Ior.Right(b)   => Some(b)
      case Ior.Both(_, b) => Some(b)
      case Ior.Left(_)    => None
    }

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].onlyLeft
   * res0: Option[String] = Some(abc)
   *
   * scala> 123.rightIor[String].onlyLeft
   * res1: Option[String] = None
   *
   * scala> Ior.Both("abc", 123).onlyLeft
   * res2: Option[String] = None
   * }}}
   */
  final def onlyLeft: Option[A] = fold(a => Some(a), _ => None, (_, _) => None)

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].onlyRight
   * res0: Option[Int] = None
   *
   * scala> 123.rightIor[String].onlyRight
   * res1: Option[Int] = Some(123)
   *
   * scala> Ior.Both("abc", 123).onlyRight
   * res2: Option[Int] = None
   * }}}
   */
  final def onlyRight: Option[B] = fold(_ => None, b => Some(b), (_, _) => None)

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].onlyLeftOrRight
   * res0: Option[Either[String, Int]] = Some(Left(abc))
   *
   * scala> 123.rightIor[String].onlyLeftOrRight
   * res1: Option[Either[String, Int]] = Some(Right(123))
   *
   * scala> Ior.Both("abc", 123).onlyLeftOrRight
   * res2: Option[Either[String, Int]] = None
   * }}}
   */
  final def onlyLeftOrRight: Option[Either[A, B]] = fold(a => Some(Left(a)), b => Some(Right(b)), (_, _) => None)

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].onlyBoth
   * res0: Option[(String, Int)] = None
   *
   * scala> 123.rightIor[String].onlyBoth
   * res1: Option[(String, Int)] = None
   *
   * scala> Ior.Both("abc", 123).onlyBoth
   * res2: Option[(String, Int)] = Some((abc,123))
   * }}}
   */
  final def onlyBoth: Option[(A, B)] = fold(_ => None, _ => None, (a, b) => Some((a, b)))

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].pad
   * res0: (Option[String], Option[Int]) = (Some(abc),None)
   *
   * scala> 123.rightIor[String].pad
   * res1: (Option[String], Option[Int]) = (None,Some(123))
   *
   * scala> Ior.Both("abc", 123).pad
   * res2: (Option[String], Option[Int]) = (Some(abc),Some(123))
   * }}}
   */
  final def pad: (Option[A], Option[B]) = fold(a => (Some(a), None), b => (None, Some(b)), (a, b) => (Some(a), Some(b)))

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].unwrap
   * res0: Either[Either[String, Int], (String, Int)] = Left(Left(abc))
   *
   * scala> 123.rightIor[String].unwrap
   * res1: Either[Either[String, Int], (String, Int)] = Left(Right(123))
   *
   * scala> Ior.Both("abc", 123).unwrap
   * res2: Either[Either[String, Int], (String, Int)] = Right((abc,123))
   * }}}
   */
  final def unwrap: Either[Either[A, B], (A, B)] =
    fold(a => Left(Left(a)), b => Left(Right(b)), (a, b) => Right((a, b)))

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].toIorNes
   * res0: IorNes[String, Int] = Left(TreeSet(abc))
   *
   * scala> 123.rightIor[String].toIorNes
   * res1: IorNes[String, Int]= Right(123)
   *
   * scala> Ior.Both("abc", 123).toIorNes
   * res2: IorNes[String, Int] = Both(TreeSet(abc),123)
   * }}}
   */
  final def toIorNes[AA >: A](implicit O: Order[AA]): IorNes[AA, B] = leftMap(NonEmptySet.one(_))

  final def toIorNec[AA >: A]: IorNec[AA, B] = leftMap(NonEmptyChain.one)

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].toIorNel
   * res0: IorNel[String, Int] = Left(NonEmptyList(abc))
   *
   * scala> 123.rightIor[String].toIorNel
   * res1: IorNel[String, Int]= Right(123)
   *
   * scala> Ior.Both("abc", 123).toIorNel
   * res2: IorNel[String, Int] = Both(NonEmptyList(abc),123)
   * }}}
   */
  final def toIorNel[AA >: A]: IorNel[AA, B] = leftMap(NonEmptyList.one)

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].toEither
   * res0: Either[String, Int] = Left(abc)
   *
   * scala> 123.rightIor[String].toEither
   * res1: Either[String, Int]= Right(123)
   *
   * scala> Ior.Both("abc", 123).toEither
   * res2: Either[String, Int] = Right(123)
   * }}}
   */
  final def toEither: Either[A, B] = fold(Left(_), Right(_), (_, b) => Right(b))

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].toValidated
   * res0: Validated[String, Int] = Invalid(abc)
   *
   * scala> 123.rightIor[String].toValidated
   * res1: Validated[String, Int]= Valid(123)
   *
   * scala> Ior.Both("abc", 123).toValidated
   * res2: Validated[String, Int] = Valid(123)
   * }}}
   */
  final def toValidated: Validated[A, B] = fold(Invalid(_), Valid(_), (_, b) => Valid(b))

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].toOption
   * res0: Option[Int] = None
   *
   * scala> 123.rightIor[String].toOption
   * res1: Option[Int]= Some(123)
   *
   * scala> Ior.Both("abc", 123).toOption
   * res2: Option[Int] = Some(123)
   * }}}
   */
  final def toOption: Option[B] = right

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].toList
   * res0: List[Int] = List()
   *
   * scala> 123.rightIor[String].toList
   * res1: List[Int]= List(123)
   *
   * scala> Ior.Both("abc", 123).toList
   * res2: List[Int] = List(123)
   * }}}
   */
  final def toList: List[B] = right.toList

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].to[List, Int]
   * res0: List[Int] = List()
   *
   * scala> 123.rightIor[String].to[List, Int]
   * res1: List[Int]= List(123)
   *
   * scala> Ior.Both("abc", 123).to[List, Int]
   * res2: List[Int] = List(123)
   * }}}
   */
  final def to[F[_], BB >: B](implicit F: Alternative[F]): F[BB] =
    fold(_ => F.empty, F.pure, (_, b) => F.pure(b))

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].swap
   * res0: Ior[Int, String] = Right(abc)
   *
   * scala> 123.rightIor[String].swap
   * res1: Ior[Int, String] = Left(123)
   *
   * scala> Ior.Both("abc", 123).swap
   * res2: Ior[Int, String] = Both(123,abc)
   * }}}
   */
  final def swap: B Ior A = fold(Ior.right, Ior.left, (a, b) => Ior.both(b, a))

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].exists(_ > 100)
   * res0: Boolean = false
   *
   * scala> 123.rightIor[String].exists(_ > 100)
   * res1: Boolean = true
   *
   * scala> Ior.Both("abc", 123).exists(_ > 100)
   * res2: Boolean = true
   * }}}
   */
  final def exists(p: B => Boolean): Boolean = right.exists(p)

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].forall(_ > 100)
   * res0: Boolean = true
   *
   * scala> 123.rightIor[String].forall(_ > 150)
   * res1: Boolean = false
   *
   * scala> Ior.Both("abc", 123).forall(_ > 100)
   * res2: Boolean = true
   * }}}
   */
  final def forall(p: B => Boolean): Boolean = right.forall(p)

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].getOrElse(456)
   * res0: Int = 456
   *
   * scala> 123.rightIor[String].getOrElse(456)
   * res1: Int = 123
   *
   * scala> Ior.Both("abc", 123).getOrElse(456)
   * res2: Int = 123
   * }}}
   */
  final def getOrElse[BB >: B](bb: => BB): BB = right.getOrElse(bb)

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].valueOr(_.length)
   * res0: Int = 3
   *
   * scala> 123.rightIor[String].valueOr(_.length)
   * res1: Int = 123
   *
   * scala> Ior.Both("abc", 123).valueOr(_.length)
   * res2: Int = 126
   * }}}
   */
  final def valueOr[BB >: B](f: A => BB)(implicit BB: Semigroup[BB]): BB =
    fold(f, identity, (a, b) => BB.combine(f(a), b))

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].bimap(_.length, identity)
   * res0: Ior[Int, Int] = Left(3)
   *
   * scala> 123.rightIor[String].bimap(_.length, identity)
   * res1: Ior[Int, Int] = Right(123)
   *
   * scala> Ior.Both("abc", 123).bimap(_.length, identity)
   * res2: Ior[Int, Int] = Both(3,123)
   * }}}
   */
  final def bimap[C, D](fa: A => C, fb: B => D): C Ior D =
    fold(a => Ior.left(fa(a)), b => Ior.right(fb(b)), (a, b) => Ior.both(fa(a), fb(b)))

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].map(_ * 2)
   * res0: Ior[String, Int] = Left(abc)
   *
   * scala> 123.rightIor[String].map(_ * 2)
   * res1: Ior[String, Int] = Right(246)
   *
   * scala> Ior.Both("abc", 123).map(_ * 2)
   * res2: Ior[String, Int] = Both(abc,246)
   * }}}
   */
  final def map[D](f: B => D): A Ior D = bimap(identity, f)

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].leftMap(_.length)
   * res0: Ior[Int, Int] = Left(3)
   *
   * scala> 123.rightIor[String].leftMap(_.length)
   * res1: Ior[Int, Int] = Right(123)
   *
   * scala> Ior.Both("abc", 123).leftMap(_.length)
   * res2: Ior[Int, Int] = Both(3,123)
   * }}}
   */
  final def leftMap[C](f: A => C): C Ior B = bimap(f, identity)

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].flatMap(i => 456.rightIor[String])
   * res0: Ior[String, Int] = Left(abc)
   *
   * scala> 123.rightIor[String].flatMap(i => (i * 2).rightIor[String])
   * res1: Ior[String, Int] = Right(246)
   *
   * scala> 123.rightIor[String].flatMap(_ => "error".leftIor[Int])
   * res2: Ior[String, Int] = Left(error)
   *
   * scala> Ior.Both("abc", 123).flatMap(_ => 456.rightIor[String])
   * res3: Ior[String, Int] = Both(abc,456)
   *
   * scala> Ior.Both("abc", 123).flatMap(_ => "error".leftIor[Int])
   * res4: Ior[String, Int] = Left(abcerror)
   *
   * scala> Ior.Both("abc", 123).flatMap(_ => Ior.Both("error",456))
   * res5: Ior[String, Int] = Both(abcerror,456)
   * }}}
   */
  final def flatMap[AA >: A, D](f: B => AA Ior D)(implicit AA: Semigroup[AA]): AA Ior D =
    this match {
      case l @ Ior.Left(_) => l
      case Ior.Right(b)    => f(b)
      case Ior.Both(a1, b) =>
        f(b) match {
          case Ior.Left(a2)    => Ior.Left(AA.combine(a1, a2))
          case Ior.Right(b)    => Ior.Both(a1, b)
          case Ior.Both(a2, d) => Ior.Both(AA.combine(a1, a2), d)
        }
    }

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * // Nothing to show
   * scala> "abc".leftIor[Int].foreach(println)
   * res0: Unit = ()
   *
   * // 123 to be shown
   * scala> 123.rightIor[String].foreach(println)
   * res1: Unit = ()
   *
   * 123 to be shown
   * scala> Ior.both("abc", 123).foreach(println)
   * res2: Unit = ()
   * }}}
   */
  final def foreach(f: B => Unit): Unit = {
    bimap(_ => (), f)
    ()
  }

  /**
   * Example
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].traverse(i => List(i, i * 2))
   * res0: List[Ior[String,Int]] = List(Left(abc))
   *
   * scala> 123.rightIor[String].traverse(i => List(i, i * 2))
   * res1: List[Ior[String,Int]] = List(Right(123), Right(246))
   *
   * scala> Ior.both("abc", 123).traverse(i => List(i, i * 2))
   * res2: List[Ior[String,Int]] = List(Both(abc,123), Both(abc,246))
   * }}}
   */
  final def traverse[F[_], AA >: A, D](g: B => F[D])(implicit F: Applicative[F]): F[AA Ior D] =
    this match {
      case Ior.Left(a)    => F.pure(Ior.left(a))
      case Ior.Right(b)   => F.map(g(b))(Ior.right)
      case Ior.Both(a, b) => F.map(g(b))(d => Ior.both(a, d))
    }

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].foldLeft(List(456))((c,b) => b :: c)
   * res0: List[Int] = List(456)
   *
   * scala> 123.rightIor[String].foldLeft(List(456))((c,b) => b :: c)
   * res1: List[Int] = List(123, 456)
   *
   * scala> Ior.Both("abc", 123).foldLeft(List(456))((c,b) => b :: c)
   * res2: List[Int] = List(123, 456)
   * }}}
   */
  final def foldLeft[C](c: C)(f: (C, B) => C): C =
    fold(_ => c, f(c, _), (_, b) => f(c, b))

  final def foldRight[C](lc: Eval[C])(f: (B, Eval[C]) => Eval[C]): Eval[C] =
    fold(_ => lc, f(_, lc), (_, b) => f(b, lc))

  final def merge[AA >: A](implicit ev: B <:< AA, AA: Semigroup[AA]): AA =
    fold(identity, ev, (a, b) => AA.combine(a, b))
  final def mergeLeft[AA >: A](implicit ev: B <:< AA): AA =
    fold(identity, ev, (a, _) => a)
  final def mergeRight[AA >: A](implicit ev: B <:< AA): AA =
    fold(identity, ev, (_, b) => ev(b))
  final def mergeWith[AA >: A](f: (A, B) => AA)(implicit ev: B <:< AA): AA =
    fold(identity, ev, f)

  /**
   * Example:
   * {{{
   * scala> import cats.data.Ior
   * scala> import cats.syntax.all._
   *
   * scala> "abc".leftIor[Int].combine("other".leftIor[Int])
   * res0: Ior[String, Int] = Left(abcother)
   *
   * scala> "abc".leftIor[Int].combine(456.rightIor[String])
   * res1: Ior[String, Int] = Both(abc,456)
   *
   * scala> 123.rightIor[String].combine("other".leftIor[Int])
   * res2: Ior[String, Int] = Both(other,123)
   *
   * scala> Ior.Both("abc", 123).combine(Ior.Both("other",456))
   * res3: Ior[String, Int] = Both(abcother,579)
   *
   * scala> Ior.Both("abc", 123).combine("other".leftIor[Int])
   * res3: Ior[String, Int] = Both(abcother,123)
   *
   * scala> Ior.Both("abc", 123).combine(456.rightIor[String])
   * res3: Ior[String, Int] = Both(abc,579)
   * }}}
   */
  final def combine[AA >: A, BB >: B](that: AA Ior BB)(implicit AA: Semigroup[AA], BB: Semigroup[BB]): AA Ior BB =
    this match {
      case Ior.Left(a1) =>
        that match {
          case Ior.Left(a2)     => Ior.Left(AA.combine(a1, a2))
          case Ior.Right(b2)    => Ior.Both(a1, b2)
          case Ior.Both(a2, b2) => Ior.Both(AA.combine(a1, a2), b2)
        }
      case Ior.Right(b1) =>
        that match {
          case Ior.Left(a2)     => Ior.Both(a2, b1)
          case Ior.Right(b2)    => Ior.Right(BB.combine(b1, b2))
          case Ior.Both(a2, b2) => Ior.Both(a2, BB.combine(b1, b2))
        }
      case Ior.Both(a1, b1) =>
        that match {
          case Ior.Left(a2)     => Ior.Both(AA.combine(a1, a2), b1)
          case Ior.Right(b2)    => Ior.Both(a1, BB.combine(b1, b2))
          case Ior.Both(a2, b2) => Ior.Both(AA.combine(a1, a2), BB.combine(b1, b2))
        }
    }

  final def ===[AA >: A, BB >: B](that: AA Ior BB)(implicit AA: Eq[AA], BB: Eq[BB]): Boolean =
    (this, that) match {
      case (Ior.Left(a), Ior.Left(aa))        => AA.eqv(a, aa)
      case (Ior.Right(b), Ior.Right(bb))      => BB.eqv(b, bb)
      case (Ior.Both(a, b), Ior.Both(aa, bb)) => AA.eqv(a, aa) && BB.eqv(b, bb)
      case _                                  => false
    }

  final def compare[AA >: A, BB >: B](that: AA Ior BB)(implicit AA: Order[AA], BB: Order[BB]): Int =
    (this, that) match {
      case (Ior.Left(a1), Ior.Left(a2))   => AA.compare(a1, a2)
      case (Ior.Left(_), _)               => -1
      case (Ior.Right(b1), Ior.Right(b2)) => BB.compare(b1, b2)
      case (Ior.Right(_), Ior.Left(_))    => 1
      case (Ior.Right(_), Ior.Both(_, _)) => -1
      case (Ior.Both(a1, b1), Ior.Both(a2, b2)) => {
        val r = AA.compare(a1, a2)
        if (r == 0) BB.compare(b1, b2) else r
      }
      case (Ior.Both(_, _), _) => 1
    }

  final def show[AA >: A, BB >: B](implicit AA: Show[AA], BB: Show[BB]): String =
    fold(
      a => s"Ior.Left(${AA.show(a)})",
      b => s"Ior.Right(${BB.show(b)})",
      (a, b) => s"Ior.Both(${AA.show(a)}, ${BB.show(b)})"
    )
}

object Ior extends IorInstances with IorFunctions with IorFunctions2 {
  final case class Left[+A](a: A) extends (A Ior Nothing)
  final case class Right[+B](b: B) extends (Nothing Ior B)
  final case class Both[+A, +B](a: A, b: B) extends (A Ior B)
}

sealed abstract private[data] class IorInstances extends IorInstances0 {

  implicit val catsBitraverseForIor: Bitraverse[Ior] = new Bitraverse[Ior] {

    def bitraverse[G[_], A, B, C, D](
      fab: Ior[A, B]
    )(f: A => G[C], g: B => G[D])(implicit G: Applicative[G]): G[Ior[C, D]] =
      fab match {
        case Ior.Left(a)    => G.map(f(a))(Ior.Left(_))
        case Ior.Right(b)   => G.map(g(b))(Ior.Right(_))
        case Ior.Both(a, b) => G.map2(f(a), g(b))(Ior.Both(_, _))
      }

    def bifoldLeft[A, B, C](fab: Ior[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
      fab match {
        case Ior.Left(a)    => f(c, a)
        case Ior.Right(b)   => g(c, b)
        case Ior.Both(a, b) => g(f(c, a), b)
      }

    def bifoldRight[A, B, C](fab: Ior[A, B],
                             c: Eval[C]
    )(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
      fab match {
        case Ior.Left(a)    => f(a, c)
        case Ior.Right(b)   => g(b, c)
        case Ior.Both(a, b) => g(b, f(a, c))
      }
  }

  implicit def catsDataOrderForIor[A: Order, B: Order]: Order[A Ior B] = _ compare _

  implicit def catsDataShowForIor[A: Show, B: Show]: Show[A Ior B] = _.show

  implicit def catsDataSemigroupForIor[A: Semigroup, B: Semigroup]: Semigroup[Ior[A, B]] = _ combine _

  implicit def catsDataMonadErrorForIor[A: Semigroup]: MonadError[Ior[A, *], A] =
    new MonadError[Ior[A, *], A] {

      def raiseError[B](e: A): Ior[A, B] = Ior.left(e)

      def handleErrorWith[B](fa: Ior[A, B])(f: (A) => Ior[A, B]): Ior[A, B] =
        fa match {
          case Ior.Left(e) => f(e)
          case _           => fa
        }

      def flatMap[B, C](fa: Ior[A, B])(f: B => Ior[A, C]): Ior[A, C] = fa.flatMap(f)

      override def map2Eval[B, C, Z](fa: Ior[A, B], fb: Eval[Ior[A, C]])(f: (B, C) => Z): Eval[Ior[A, Z]] =
        fa match {
          case l @ Ior.Left(_) => Eval.now(l) // no need to evaluate fb
          case notLeft         => fb.map(fb => map2(notLeft, fb)(f))
        }

      def tailRecM[B, C](b: B)(fn: B => Ior[A, Either[B, C]]): A Ior C = {
        @tailrec
        def loop(v: Ior[A, Either[B, C]]): A Ior C =
          v match {
            case Ior.Left(a)           => Ior.left(a)
            case Ior.Right(Right(c))   => Ior.right(c)
            case Ior.Both(a, Right(c)) => Ior.both(a, c)
            case Ior.Right(Left(b))    => loop(fn(b))
            case Ior.Both(a, Left(b)) =>
              fn(b) match {
                case Ior.Left(aa)    => Ior.left(Semigroup[A].combine(a, aa))
                case Ior.Both(aa, x) => loop(Ior.both(Semigroup[A].combine(a, aa), x))
                case Ior.Right(x)    => loop(Ior.both(a, x))
              }
          }
        loop(fn(b))
      }

      override def pure[B](x: B): Ior[A, B] = Ior.right(x)

      override def map[B, C](fa: A Ior B)(f: B => C): A Ior C =
        fa.map(f)
    }

  implicit def catsDataBifunctorForIor: Bifunctor[Ior] =
    new Bifunctor[Ior] {
      override def bimap[A, B, C, D](fab: A Ior B)(f: A => C, g: B => D): C Ior D = fab.bimap(f, g)
    }

  implicit def catsDataParallelForIor[E](implicit E: Semigroup[E]): Parallel.Aux[Ior[E, *], Ior[E, *]] =
    new Parallel[Ior[E, *]] {
      type F[x] = Ior[E, x]

      private[this] val identityK: Ior[E, *] ~> Ior[E, *] = FunctionK.id

      def parallel: Ior[E, *] ~> Ior[E, *] = identityK
      def sequential: Ior[E, *] ~> Ior[E, *] = identityK

      val applicative: Applicative[Ior[E, *]] = new Applicative[Ior[E, *]] {
        def pure[A](a: A): Ior[E, A] = Ior.right(a)
        def ap[A, B](ff: Ior[E, A => B])(fa: Ior[E, A]): Ior[E, B] =
          fa match {
            case Ior.Right(a) =>
              ff match {
                case Ior.Right(f)    => Ior.Right(f(a))
                case Ior.Both(e1, f) => Ior.Both(e1, f(a))
                case Ior.Left(e1)    => Ior.Left(e1)
              }
            case Ior.Both(e1, a) =>
              ff match {
                case Ior.Right(f)    => Ior.Both(e1, f(a))
                case Ior.Both(e2, f) => Ior.Both(E.combine(e2, e1), f(a))
                case Ior.Left(e2)    => Ior.Left(E.combine(e2, e1))
              }
            case Ior.Left(e1) =>
              ff match {
                case Ior.Left(e2)    => Ior.Left(E.combine(e2, e1))
                case Ior.Both(e2, _) => Ior.Left(E.combine(e2, e1))
                case Ior.Right(_)    => Ior.Left(e1)
              }
          }
      }

      lazy val monad: Monad[Ior[E, *]] = Monad[Ior[E, *]]
    }

}

sealed abstract private[data] class IorInstances0 {

  implicit def catsDataTraverseFunctorForIor[A]: Traverse[Ior[A, *]] =
    new Traverse[Ior[A, *]] {
      def traverse[F[_]: Applicative, B, C](fa: A Ior B)(f: B => F[C]): F[A Ior C] =
        fa.traverse(f)

      override def mapAccumulate[S, B, C](init: S, fa: Ior[A, B])(f: (S, B) => (S, C)): (S, Ior[A, C]) =
        fa match {
          case l @ Ior.Left(_) => (init, l)
          case Ior.Right(b) =>
            val (snext, c) = f(init, b)
            (snext, Ior.Right(c))
          case Ior.Both(a, b) =>
            val (snext, c) = f(init, b)
            (snext, Ior.Both(a, c))
        }
      def foldLeft[B, C](fa: A Ior B, b: C)(f: (C, B) => C): C =
        fa.foldLeft(b)(f)
      def foldRight[B, C](fa: A Ior B, lc: Eval[C])(f: (B, Eval[C]) => Eval[C]): Eval[C] =
        fa.foldRight(lc)(f)

      override def size[B](fa: A Ior B): Long =
        if (fa.isLeft) 0L else 1L

      override def get[B](fa: A Ior B)(idx: Long): Option[B] =
        if (idx == 0L) fa.toOption else None

      override def forall[B](fa: Ior[A, B])(p: (B) => Boolean): Boolean = fa.forall(p)

      override def exists[B](fa: Ior[A, B])(p: (B) => Boolean): Boolean = fa.exists(p)

      override def map[B, C](fa: A Ior B)(f: B => C): A Ior C =
        fa.map(f)
    }

  implicit def catsDataEqForIor[A: Eq, B: Eq]: Eq[A Ior B] = _ === _
}

sealed private[data] trait IorFunctions {
  def left[A, B](a: A): A Ior B = Ior.Left(a)
  def right[A, B](b: B): A Ior B = Ior.Right(b)
  def both[A, B](a: A, b: B): A Ior B = Ior.Both(a, b)
  def leftNel[A, B](a: A): IorNel[A, B] = left(NonEmptyList.one(a))
  def bothNel[A, B](a: A, b: B): IorNel[A, B] = both(NonEmptyList.one(a), b)

  /**
   * Create an `Ior` from two Options if at least one of them is defined.
   *
   * @param oa an element (optional) for the left side of the `Ior`
   * @param ob an element (optional) for the right side of the `Ior`
   *
   * @return `None` if both `oa` and `ob` are `None`. Otherwise `Some` wrapping
   * an [[Ior.Left]], [[Ior.Right]], or [[Ior.Both]] if `oa`, `ob`, or both are
   * defined (respectively).
   *
   * Example:
   * {{{
   * scala> Ior.fromOptions(Option.empty[String], Option.empty[Int])
   * res0: Option[Ior[String, Int]] = None
   * scala> Ior.fromOptions(Option.empty[String], Some(42))
   * res1: Option[Ior[String, Int]] = Some(Right(42))
   * scala> Ior.fromOptions(Some("Error"), Option.empty[Int])
   * res2: Option[Ior[String, Int]] = Some(Left(Error))
   * scala> Ior.fromOptions(Some("Warning"), Some(42))
   * res3: Option[Ior[String, Int]] = Some(Both(Warning,42))
   * }}}
   */
  def fromOptions[A, B](oa: Option[A], ob: Option[B]): Option[A Ior B] =
    oa match {
      case Some(a) =>
        ob match {
          case Some(b) => Some(Ior.Both(a, b))
          case None    => Some(Ior.Left(a))
        }
      case None =>
        ob match {
          case Some(b) => Some(Ior.Right(b))
          case None    => None
        }
    }

  /**
   * Create an `Ior` from an `Either`.
   * @param eab an `Either` from which the `Ior` should be created
   *
   * @return [[Ior.Left]] if the `Either` was a `Left`,
   *         or [[Ior.Right]] if the `Either` was a `Right`
   *
   * Example:
   * {{{
   * scala> Ior.fromEither(Left(1))
   * res0: Ior[Int, Nothing] = Left(1)
   * scala> Ior.fromEither(Right('1'))
   * res1: Ior[Nothing, Char] = Right(1)
   * }}}
   */
  def fromEither[A, B](eab: Either[A, B]): A Ior B =
    eab match {
      case Left(a)  => left(a)
      case Right(b) => right(b)
    }
}

sealed private[data] trait IorFunctions2 {
  def leftNec[A, B](a: A): IorNec[A, B] = Ior.left(NonEmptyChain.one(a))
  def bothNec[A, B](a: A, b: B): IorNec[A, B] = Ior.both(NonEmptyChain.one(a), b)
}
