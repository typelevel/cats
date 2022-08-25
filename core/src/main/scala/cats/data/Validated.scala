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

import cats.data.Validated.{Invalid, Valid}
import cats.kernel.CommutativeSemigroup

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

sealed abstract class Validated[+E, +A] extends Product with Serializable {

  /**
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1= "error".invalid[Option[String]]
   * scala> val v2= Some("abc").valid[String]
   *
   * scala> v1.fold(identity,_.getOrElse(""))
   * res0: String = error
   *
   * scala> v2.fold(identity,_.getOrElse(""))
   * res1: String = abc
   * }}}
   */
  def fold[B](fe: E => B, fa: A => B): B =
    this match {
      case Invalid(e) => fe(e)
      case Valid(a)   => fa(a)
    }

  /**
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val validated= "error".invalid[Unit]
   * scala> validated.isValid
   * res0: Boolean = false
   * }}}
   */
  def isValid: Boolean =
    this match {
      case Invalid(_) => false
      case _          => true
    }

  /**
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val validated= "error".invalid[Unit]
   * scala> validated.isInvalid
   * res0: Boolean = true
   * }}}
   */
  def isInvalid: Boolean =
    this match {
      case Invalid(_) => true
      case _          => false
    }

  /**
   * Run the side-effecting function on the value if it is Valid
   */
  def foreach(f: A => Unit): Unit =
    this match {
      case Valid(a) => f(a)
      case _        => ()
    }

  /**
   * Return the Valid value, or the default if Invalid
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1= "error".invalid[Int]
   * scala> val v2= 123.valid[String]
   *
   * scala> v1.getOrElse(456)
   * res0: Int = 456
   *
   * scala> v2.getOrElse(456)
   * res1: Int = 123
   * }}}
   */
  def getOrElse[B >: A](default: => B): B =
    this match {
      case Valid(a) => a
      case _        => default
    }

  /**
   * Return the Valid value, or the result of f if Invalid
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = Some("exception").invalid[String]
   * scala> val v2 = "OK".valid[Option[String]]
   *
   * scala> v1.valueOr(_.getOrElse(""))
   * res0: String = exception
   *
   * scala> v2.valueOr(_.getOrElse(""))
   * res1: String = OK
   * }}}
   */
  def valueOr[B >: A](f: E => B): B =
    this match {
      case Invalid(e) => f(e)
      case Valid(a)   => a
    }

  /**
   * Is this Valid and matching the given predicate
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = List("error").invalid[Int]
   * scala> val v2 = 123.valid[List[String]]
   *
   * scala> v1.exists(_ > 120)
   * res0: Boolean = false
   *
   * scala> v2.exists(_ > 120)
   * res1: Boolean = true
   * }}}
   */
  def exists(predicate: A => Boolean): Boolean =
    this match {
      case Valid(a) => predicate(a)
      case _        => false
    }

  /**
   * Is this Invalid or matching the predicate
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = Some("error").invalid[Int]
   * scala> val v2 = 123.valid[Option[String]]
   *
   * scala> v1.forall(_ > 150)
   * res0: Boolean = true
   *
   * scala> v2.forall(_ > 150)
   * res1: Boolean = false
   * }}}
   */
  def forall(f: A => Boolean): Boolean =
    this match {
      case Valid(a) => f(a)
      case _        => true
    }

  /**
   * Return this if it is Valid, or else fall back to the given default.
   * The functionality is similar to that of [[findValid]] except for failure accumulation,
   * where here only the error on the right is preserved and the error on the left is ignored.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = Some("error").invalid[Int]
   * scala> val v2 = 123.valid[Option[String]]
   * scala> val defaultValidated = 456.valid[Option[String]]
   *
   * scala> v1.orElse(defaultValidated)
   * res0: Validated[Option[String], Int] = Valid(456)
   *
   * scala> v2.orElse(defaultValidated)
   * res1: Validated[Option[String], Int] = Valid(123)
   * }}}
   */
  def orElse[EE, AA >: A](default: => Validated[EE, AA]): Validated[EE, AA] =
    this match {
      case v @ Valid(_) => v
      case _            => default
    }

  /**
   * If `this` is valid return `this`, otherwise if `that` is valid return `that`, otherwise combine the failures.
   * This is similar to [[orElse]] except that here failures are accumulated.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = List("error1").invalid[Int]
   * scala> val v2 = 123.valid[List[String]]
   * scala> val default1 = List("error2").invalid[Int]
   * scala> val default2 = 456.valid[List[String]]
   *
   * scala> v1.findValid(default1)
   * res0: Validated[List[String], Int] = Invalid(List(error1, error2))
   *
   * scala> v1.findValid(default2)
   * res1: Validated[List[String], Int] = Valid(456)
   *
   * scala> v2.findValid(default1)
   * res2: Validated[List[String], Int] = Valid(123)
   * }}}
   */
  def findValid[EE >: E, AA >: A](that: => Validated[EE, AA])(implicit EE: Semigroup[EE]): Validated[EE, AA] =
    this match {
      case v @ Valid(_) => v
      case Invalid(e) =>
        that match {
          case v @ Valid(_) => v
          case Invalid(ee)  => Invalid(EE.combine(e, ee))
        }
    }

  /**
   * Converts the value to an Either[E, A]
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = "error".invalid[Int]
   * scala> val v2 = 123.valid[String]
   *
   * scala> v1.toEither
   * res0: Either[String, Int] = Left(error)
   *
   * scala> v2.toEither
   * res1: Either[String, Int] = Right(123)
   * }}}
   */
  def toEither: Either[E, A] =
    this match {
      case Invalid(e) => Left(e)
      case Valid(a)   => Right(a)
    }

  /**
   * Returns Valid values wrapped in Some, and None for Invalid values
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = List("error").invalid[Int]
   * scala> val v2 = 123.valid[List[String]]
   *
   * scala> v1.toOption
   * res0: Option[Int] = None
   *
   * scala> v2.toOption
   * res1: Option[Int] = Some(123)
   * }}}
   */
  def toOption: Option[A] =
    this match {
      case Valid(a) => Some(a)
      case _        => None
    }

  /**
   * Returns Valid values wrapped in Ior.Right, and None for Ior.Left values
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = "error".invalid[Int]
   * scala> val v2 = 123.valid[String]
   *
   * scala> v1.toIor
   * res0: Ior[String, Int] = Left(error)
   *
   * scala> v2.toIor
   * res1: Ior[String, Int] = Right(123)
   * }}}
   */
  def toIor: Ior[E, A] =
    this match {
      case Invalid(e) => Ior.Left(e)
      case Valid(a)   => Ior.Right(a)
    }

  /**
   * Convert this value to a single element List if it is Valid,
   * otherwise return an empty List
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = Some("error").invalid[Int]
   * scala> val v2 = 123.valid[Option[String]]
   *
   * scala> v1.toList
   * res0: List[Int] = List()
   *
   * scala> v2.toList
   * res1: List[Int] = List(123)
   * }}}
   */
  def toList: List[A] =
    this match {
      case Valid(a) => List(a)
      case _        => Nil
    }

  /**
   * Lift the Invalid value into a NonEmptyList.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = "error".invalid[Int]
   * scala> val v2 = 123.valid[String]
   *
   * scala> v1.toValidatedNel
   * res0: ValidatedNel[String, Int] = Invalid(NonEmptyList(error))
   *
   * scala> v2.toValidatedNel
   * res1: ValidatedNel[String, Int] = Valid(123)
   * }}}
   */
  def toValidatedNel[EE >: E, AA >: A]: ValidatedNel[EE, AA] =
    this match {
      case v @ Valid(_) => v
      case Invalid(e)   => Validated.invalidNel(e)
    }

  /**
   * Lift the Invalid value into a NonEmptyChain.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = "error".invalid[Int]
   * scala> val v2 = 123.valid[String]
   *
   * scala> v1.toValidatedNec
   * res0: ValidatedNec[String, Int] = Invalid(Chain(error))
   *
   * scala> v2.toValidatedNec
   * res1: ValidatedNec[String, Int] = Valid(123)
   * }}}
   */
  def toValidatedNec[EE >: E, AA >: A]: ValidatedNec[EE, AA] =
    this match {
      case v @ Valid(_) => v
      case Invalid(e)   => Validated.invalidNec(e)
    }

  /**
   * Convert to an Either, apply a function, convert back.  This is handy
   * when you want to use the Monadic properties of the Either type.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = "error".invalid[Int]
   * scala> val v2 = 123.valid[String]
   *
   * scala> v1.withEither(_.bimap(List(_), Option(_)))
   * res0: Validated[List[String], Option[Int]] = Invalid(List(error))
   *
   * scala> v2.withEither(_.bimap(List(_), Option(_)))
   * res1: Validated[List[String], Option[Int]] = Valid(Some(123))
   * }}}
   */
  def withEither[EE, B](f: Either[E, A] => Either[EE, B]): Validated[EE, B] =
    Validated.fromEither(f(toEither))

  /**
   * Validated is a [[Bifunctor]], this method applies one of the
   * given functions.]
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1: Validated[String, Int] = "error".invalid[Int]
   * scala> val v2: Validated[String, Int] = 123.valid[String]
   *
   * scala> v1.bimap(List(_), Option(_))
   * res0: Validated[List[String], Option[Int]] = Invalid(List(error))
   *
   * scala> v2.bimap(List(_), Option(_))
   * res1: Validated[List[String] ,Option[Int]] = Valid(Some(123))
   * }}}
   */
  def bimap[EE, AA](fe: E => EE, fa: A => AA): Validated[EE, AA] =
    this match {
      case Valid(a)   => Valid(fa(a))
      case Invalid(e) => Invalid(fe(e))
    }

  /**
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = "error".invalid[Int]
   * scala> val v2 = "error2".invalid[Int]
   * scala> val v3 = 123.valid[String]
   * scala> val v4 = 456.valid[String]
   * scala> val v5 = v4
   *
   * scala> v1 compare v2
   * res0: Int = -1
   *
   * scala> v1 compare v3
   * res1: Int = -1
   *
   * scala> v3 compare v1
   * res3: Int = 1
   *
   * scala> v3 compare v4
   * res4: Int = -1
   *
   * scala> v4 compare v5
   * res5: Int = 0
   * }}}
   */
  def compare[EE >: E, AA >: A](that: Validated[EE, AA])(implicit EE: Order[EE], AA: Order[AA]): Int =
    (this, that) match {
      case (Valid(a), Valid(aa))     => AA.compare(a, aa)
      case (Invalid(e), Invalid(ee)) => EE.compare(e, ee)
      case (Invalid(_), _)           => -1
      case (Valid(_), _)             => 1
    }

  def partialCompare[EE >: E, AA >: A](
    that: Validated[EE, AA]
  )(implicit EE: PartialOrder[EE], AA: PartialOrder[AA]): Double =
    (this, that) match {
      case (Valid(a), Valid(aa))     => AA.partialCompare(a, aa)
      case (Invalid(e), Invalid(ee)) => EE.partialCompare(e, ee)
      case (Invalid(_), _)           => -1
      case (Valid(_), _)             => 1
    }

  /**
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = "error".invalid[Int]
   * scala> val v2 = "error".invalid[Int]
   * scala> val v3 = 123.valid[String]
   * scala> val v4 = 456.valid[String]
   *
   * scala> v1 === v2
   * res0: Boolean = true
   *
   * scala> v1 === v3
   * res1: Boolean = false
   *
   * scala> v3 === v4
   * res3: Boolean = false
   * }}}
   */
  def ===[EE >: E, AA >: A](that: Validated[EE, AA])(implicit EE: Eq[EE], AA: Eq[AA]): Boolean =
    (this, that) match {
      case (Invalid(e), Invalid(ee)) => EE.eqv(e, ee)
      case (Valid(a), Valid(aa))     => AA.eqv(a, aa)
      case _                         => false
    }

  /**
   * From Apply:
   * if both the function and this value are Valid, apply the function
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = "error".invalid[Int]
   * scala> val v2 = 123.valid[String]
   * scala> val f: Validated[String, Int => Option[Int]] = (Option.apply[Int] _).valid[String]
   *
   * scala> v1.ap(f)
   * res0: Validated[String, Option[Int]] = Invalid(error)
   *
   * scala> v2.ap(f)
   * res1: Validated[String, Option[Int]] = Valid(Some(123))
   * }}}
   */
  def ap[EE >: E, B](f: Validated[EE, A => B])(implicit EE: Semigroup[EE]): Validated[EE, B] =
    (this, f) match {
      case (Valid(a), Valid(f))       => Valid(f(a))
      case (Invalid(e1), Invalid(e2)) => Invalid(EE.combine(e2, e1))
      case (e @ Invalid(_), _)        => e
      case (_, e @ Invalid(_))        => e
    }

  /**
   * From Product
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.data.ValidatedNec
   *
   * scala> val v1 = "error".invalidNec[Int]
   * scala> val v2 = "error2".invalidNec[Int]
   * scala> val v3 = 123.validNec[String]
   * scala> val v4 = 456.validNec[String]
   *
   * scala> v1.product(v2)
   * res0: ValidatedNec[String, (Int, Int)] = Invalid(Chain(error, error2))
   *
   * scala> v1.product(v3)
   * res1: ValidatedNec[String, (Int, Int)] = Invalid(Chain(error))
   *
   * scala> v3.product(v4)
   * res2: ValidatedNec[String, (Int, Int)] = Valid((123,456))
   *
   * }}}
   */
  def product[EE >: E, B](fb: Validated[EE, B])(implicit EE: Semigroup[EE]): Validated[EE, (A, B)] =
    (this, fb) match {
      case (Valid(a), Valid(b))       => Valid((a, b))
      case (Invalid(e1), Invalid(e2)) => Invalid(EE.combine(e1, e2))
      case (e @ Invalid(_), _)        => e
      case (_, e @ Invalid(_))        => e
    }

  /**
   * Apply a function to a Valid value, returning a new Valid value
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = "error".invalid[Int]
   * scala> val v2 = 123.valid[String]
   *
   * scala> v1.map(_ * 2)
   * res0: Validated[String, Int] = Invalid(error)
   *
   * scala> v2.map(_ * 2)
   * res1: Validated[String, Int] = Valid(246)
   * }}}
   */
  def map[B](f: A => B): Validated[E, B] =
    this match {
      case i @ Invalid(_) => i
      case Valid(a)       => Valid(f(a))
    }

  /**
   * Apply a function to an Invalid value, returning a new Invalid value.
   * Or, if the original valid was Valid, return it.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = "error".invalid[Int]
   * scala> val v2 = 123.valid[String]
   *
   * scala> v1.leftMap(Option.apply)
   * res0: Validated[Option[String], Int] = Invalid(Some(error))
   *
   * scala> v2.leftMap(Option.apply)
   * res1: Validated[Option[String], Int] = Valid(123)
   *
   * }}}
   */
  def leftMap[EE](f: E => EE): Validated[EE, A] =
    this match {
      case a @ Valid(_) => a
      case Invalid(e)   => Invalid(f(e))
    }

  /**
   * When Valid, apply the function, marking the result as valid
   * inside the Applicative's context,
   * when Invalid, lift the Error into the Applicative's context
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = "error".invalid[Int]
   * scala> val v2 = 123.valid[String]
   *
   * scala> v1.traverse(Option.apply[Int])
   * res0: Option[Validated[String, Int]] = Some(Invalid(error))
   *
   * scala> v2.traverse(Option.apply[Int])
   * res1: Option[Validated[String, Int]] = Some(Valid(123))
   * }}}
   */
  def traverse[F[_], EE >: E, B](f: A => F[B])(implicit F: Applicative[F]): F[Validated[EE, B]] =
    this match {
      case Valid(a)       => F.map(f(a))(Valid.apply)
      case e @ Invalid(_) => F.pure(e)
    }

  def mapAccumulate[S, EE >: E, B](init: S)(f: (S, A) => (S, B)): (S, Validated[EE, B]) =
    this match {
      case Valid(a) =>
        val (snext, b) = f(init, a)
        (snext, Valid(b))

      case e @ Invalid(_) => (init, e)
    }

  /**
   * apply the given function to the value with the given B when
   * valid, otherwise return the given B
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = "error".invalid[Int]
   * scala> val v2 = 123.valid[String]
   *
   * scala> v1.foldLeft(456)(_ + _)
   * res0: Int = 456
   *
   * scala> v2.foldLeft(456)(_ + _)
   * res1: Int = 579
   * }}}
   */
  def foldLeft[B](b: B)(f: (B, A) => B): B =
    this match {
      case Valid(a) => f(b, a)
      case _        => b
    }

  /**
   * Lazily-apply the given function to the value with the given B
   * when valid, otherwise return the given B.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import cats.Eval
   *
   * scala> val v1 = "error".invalid[Int]
   * scala> val v2 = 123.valid[String]
   *
   * scala> v1.foldRight(Eval.now(456))((i,e) => e.map(_ + i))
   * res0: Eval[Int] = Now(456)
   *
   * scala> v2.foldRight(Eval.now(456))((i,e) => e.map(_ + i)).value
   * res1: Int = 579
   * }}}
   */
  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    this match {
      case Valid(a) => f(a, lb)
      case _        => lb
    }

  def show[EE >: E, AA >: A](implicit EE: Show[EE], AA: Show[AA]): String =
    this match {
      case Invalid(e) => s"Invalid(${EE.show(e)})"
      case Valid(a)   => s"Valid(${AA.show(a)})"
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
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = "error".invalid[Int]
   * scala> val v2 = 123.valid[String]
   * scala> val f: Int => Validated[String, List[Int]] = List(_).valid[String]
   *
   * scala> v1.andThen(f)
   * res0: Validated[String, List[Int]] = Invalid(error)
   *
   * scala> v2.andThen(f)
   * res1: Validated[String, List[Int]] = Valid(List(123))
   * }}}
   */
  def andThen[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] =
    this match {
      case Valid(a)       => f(a)
      case i @ Invalid(_) => i
    }

  /**
   * Combine this `Validated` with another `Validated`, using the `Semigroup`
   * instances of the underlying `E` and `A` instances. The resultant `Validated`
   * will be `Valid`, if, and only if, both this `Validated` instance and the
   * supplied `Validated` instance are also `Valid`.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = "error".invalidNel[List[Int]]
   * scala> val v2 = "error2".invalidNel[List[Int]]
   * scala> val v3 = List(123).validNel[String]
   * scala> val v4 = List(456).validNel[String]
   *
   * scala> v1 combine v2
   * res0: Validated[NonEmptyList[String], List[Int]] = Invalid(NonEmptyList(error, error2))
   *
   * scala> v2 combine v3
   * res1: Validated[NonEmptyList[String], List[Int]] = Invalid(NonEmptyList(error2))
   *
   * scala> v3 combine v4
   * res2: Validated[NonEmptyList[String], List[Int]] = Valid(List(123, 456))
   * }}}
   */
  def combine[EE >: E, AA >: A](
    that: Validated[EE, AA]
  )(implicit EE: Semigroup[EE], AA: Semigroup[AA]): Validated[EE, AA] =
    (this, that) match {
      case (Valid(a), Valid(b))     => Valid(AA.combine(a, b))
      case (Invalid(a), Invalid(b)) => Invalid(EE.combine(a, b))
      case (Invalid(_), _)          => this
      case _                        => that
    }

  /**
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = "error".invalid[Int]
   * scala> val v2 = 123.valid[String]
   *
   * scala> v1.swap
   * res0: Validated[Int, String] = Valid(error)
   *
   * scala> v2.swap
   * res1: Validated[Int, String] = Invalid(123)
   * }}}
   */
  def swap: Validated[A, E] =
    this match {
      case Valid(a)   => Invalid(a)
      case Invalid(e) => Valid(e)
    }

  /**
   * Example:
   * {{{
   * scala> import cats.implicits._
   *
   * scala> val v1 = Seq("error").invalid[List[String]]
   * scala> val v2 = List("Ok").valid[Seq[String]]
   *
   * scala> v1.merge
   * res0: Seq[String] = List(error)
   *
   * scala> v2.merge
   * res1: Seq[String] = List(Ok)
   * }}}
   */
  def merge[EE >: E](implicit ev: A <:< EE): EE =
    this match {
      case Invalid(e) => e
      case Valid(a)   => ev(a)
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
  def ensure[EE >: E](onFailure: => EE)(f: A => Boolean): Validated[EE, A] =
    this match {
      case Valid(a) => if (f(a)) this else Validated.invalid(onFailure)
      case _        => this
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
  def ensureOr[EE >: E](onFailure: A => EE)(f: A => Boolean): Validated[EE, A] =
    this match {
      case Valid(a) => if (f(a)) this else Validated.invalid(onFailure(a))
      case _        => this
    }
}

object Validated extends ValidatedInstances with ValidatedFunctions with ValidatedFunctionsBinCompat0 {
  final case class Valid[+A](a: A) extends Validated[Nothing, A]
  final case class Invalid[+E](e: E) extends Validated[E, Nothing]

  private[data] val validUnit: Validated[Nothing, Unit] = Valid(())

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
  final private[data] class CatchOnlyPartiallyApplied[T](private val dummy: Boolean = true) extends AnyVal {
    /* Note: the NT parameter is not referenced at runtime, but serves a compile-time role.
     * See https://github.com/typelevel/cats/pull/1867/files#r138381991 for details. */
    def apply[A](f: => A)(implicit T: ClassTag[T], NT: NotNull[T]): Validated[T, A] =
      try {
        valid(f)
      } catch {
        case t if T.runtimeClass.isInstance(t) =>
          invalid(t.asInstanceOf[T])
      }
  }
}

sealed abstract private[data] class ValidatedInstances extends ValidatedInstances1 {

  implicit def catsDataSemigroupKForValidated[A](implicit A: Semigroup[A]): SemigroupK[Validated[A, *]] =
    new SemigroupK[Validated[A, *]] {
      def combineK[B](x: Validated[A, B], y: Validated[A, B]): Validated[A, B] =
        x match {
          case v @ Valid(_) => v
          case Invalid(ix) =>
            y match {
              case Invalid(iy)  => Invalid(A.combine(ix, iy))
              case v @ Valid(_) => v
            }
        }
    }

  implicit def catsDataAlignForValidated[E: Semigroup]: Align[Validated[E, *]] =
    new Align[Validated[E, *]] {
      def functor: Functor[Validated[E, *]] = catsDataTraverseFunctorForValidated
      def align[A, B](fa: Validated[E, A], fb: Validated[E, B]): Validated[E, Ior[A, B]] =
        alignWith(fa, fb)(identity)

      override def alignWith[A, B, C](fa: Validated[E, A], fb: Validated[E, B])(f: Ior[A, B] => C): Validated[E, C] =
        fa match {
          case Invalid(e) =>
            fb match {
              case Invalid(e2) => Invalid(Semigroup[E].combine(e, e2))
              case Valid(b)    => Valid(f(Ior.right(b)))
            }
          case Valid(a) =>
            fb match {
              case Invalid(e) => Valid(f(Ior.left(a)))
              case Valid(b)   => Valid(f(Ior.both(a, b)))
            }
        }
    }

  implicit def catsDataMonoidForValidated[A, B](implicit A: Semigroup[A], B: Monoid[B]): Monoid[Validated[A, B]] =
    new Monoid[Validated[A, B]] {
      def empty: Validated[A, B] = Valid(B.empty)
      def combine(x: Validated[A, B], y: Validated[A, B]): Validated[A, B] = x.combine(y)
    }

  implicit def catsDataOrderForValidated[A: Order, B: Order]: Order[Validated[A, B]] =
    new Order[Validated[A, B]] {
      def compare(x: Validated[A, B], y: Validated[A, B]): Int = x.compare(y)
      override def partialCompare(x: Validated[A, B], y: Validated[A, B]): Double = x.partialCompare(y)
      override def eqv(x: Validated[A, B], y: Validated[A, B]): Boolean = x === y
    }

  implicit def catsDataShowForValidated[A: Show, B: Show]: Show[Validated[A, B]] = _.show

  implicit val catsDataBitraverseForValidated: Bitraverse[Validated] =
    new Bitraverse[Validated] {
      def bitraverse[G[_], A, B, C, D](
        fab: Validated[A, B]
      )(f: A => G[C], g: B => G[D])(implicit G: Applicative[G]): G[Validated[C, D]] =
        fab match {
          case Invalid(a) => G.map(f(a))(Validated.invalid)
          case Valid(b)   => G.map(g(b))(Validated.valid)
        }

      def bifoldLeft[A, B, C](fab: Validated[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
        fab match {
          case Invalid(a) => f(c, a)
          case Valid(b)   => g(c, b)
        }

      def bifoldRight[A, B, C](fab: Validated[A, B],
                               c: Eval[C]
      )(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
        fab match {
          case Invalid(a) => f(a, c)
          case Valid(b)   => g(b, c)
        }

      override def bimap[A, B, C, D](fab: Validated[A, B])(f: A => C, g: B => D): Validated[C, D] =
        fab.bimap(f, g)

      override def leftMap[A, B, C](fab: Validated[A, B])(f: A => C): Validated[C, B] =
        fab.leftMap(f)
    }

  implicit def catsDataApplicativeErrorForValidated[E](implicit E: Semigroup[E]): ApplicativeError[Validated[E, *], E] =
    new ValidatedApplicative[E] with ApplicativeError[Validated[E, *], E] {

      def handleErrorWith[A](fa: Validated[E, A])(f: E => Validated[E, A]): Validated[E, A] =
        fa match {
          case Validated.Invalid(e)   => f(e)
          case v @ Validated.Valid(_) => v
        }
      def raiseError[A](e: E): Validated[E, A] = Validated.Invalid(e)
    }
}

sealed abstract private[data] class ValidatedInstances1 extends ValidatedInstances2 {

  implicit def catsDataSemigroupForValidated[A: Semigroup, B: Semigroup]: Semigroup[Validated[A, B]] = _ combine _

  implicit def catsDataCommutativeApplicativeForValidated[E: CommutativeSemigroup]
    : CommutativeApplicative[Validated[E, *]] =
    new ValidatedApplicative[E] with CommutativeApplicative[Validated[E, *]]

  implicit def catsDataPartialOrderForValidated[A: PartialOrder, B: PartialOrder]: PartialOrder[Validated[A, B]] =
    new PartialOrder[Validated[A, B]] {
      def partialCompare(x: Validated[A, B], y: Validated[A, B]): Double = x.partialCompare(y)
      override def eqv(x: Validated[A, B], y: Validated[A, B]): Boolean = x === y
    }
}

sealed abstract private[data] class ValidatedInstances2 {
  implicit def catsDataEqForValidated[A: Eq, B: Eq]: Eq[Validated[A, B]] = _ === _

  implicit def catsDataTraverseFunctorForValidated[E]: Traverse[Validated[E, *]] =
    new Traverse[Validated[E, *]] {

      override def traverse[G[_]: Applicative, A, B](fa: Validated[E, A])(f: (A) => G[B]): G[Validated[E, B]] =
        fa.traverse(f)

      override def mapAccumulate[S, A, B](init: S, fa: Validated[E, A])(f: (S, A) => (S, B)): (S, Validated[E, B]) =
        fa.mapAccumulate(init)(f)

      override def foldLeft[A, B](fa: Validated[E, A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: Validated[E, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)

      override def map[A, B](fa: Validated[E, A])(f: (A) => B): Validated[E, B] =
        fa.map(f)

      override def reduceLeftToOption[A, B](fa: Validated[E, A])(f: A => B)(g: (B, A) => B): Option[B] =
        fa.map(f).toOption

      override def reduceRightToOption[A, B](
        fa: Validated[E, A]
      )(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]] =
        Now(fa.map(f).toOption)

      override def reduceLeftOption[A](fa: Validated[E, A])(f: (A, A) => A): Option[A] =
        fa.toOption

      override def reduceRightOption[A](fa: Validated[E, A])(f: (A, Eval[A]) => Eval[A]): Eval[Option[A]] =
        Now(fa.toOption)

      override def size[A](fa: Validated[E, A]): Long =
        fa match {
          case Invalid(_) => 0L
          case _          => 1L
        }

      override def get[A](fa: Validated[E, A])(idx: Long): Option[A] =
        if (idx == 0L) fa.toOption else None

      override def foldMap[A, B](fa: Validated[E, A])(f: A => B)(implicit B: Monoid[B]): B =
        fa match {
          case Valid(a) => f(a)
          case _        => B.empty
        }

      override def find[A](fa: Validated[E, A])(f: A => Boolean): Option[A] =
        fa.toOption.filter(f)

      override def exists[A](fa: Validated[E, A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: Validated[E, A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def toList[A](fa: Validated[E, A]): List[A] =
        fa match {
          case Valid(a) => a :: Nil
          case _        => Nil
        }

      override def isEmpty[A](fa: Validated[E, A]): Boolean = fa.isInvalid

      override def void[A](fa: Validated[E, A]): Validated[E, Unit] =
        if (fa.isValid) Validated.validUnit
        else fa.asInstanceOf[Validated[E, Unit]]
    }
}

private[data] class ValidatedApplicative[E: Semigroup] extends CommutativeApplicative[Validated[E, *]] {
  override def map[A, B](fa: Validated[E, A])(f: A => B): Validated[E, B] =
    fa.map(f)

  def pure[A](a: A): Validated[E, A] = Validated.valid(a)

  def ap[A, B](ff: Validated[E, (A) => B])(fa: Validated[E, A]): Validated[E, B] =
    fa.ap(ff)(Semigroup[E])

  override def product[A, B](fa: Validated[E, A], fb: Validated[E, B]): Validated[E, (A, B)] =
    fa.product(fb)(Semigroup[E])

  override def unit: Validated[E, Unit] = Validated.validUnit
}

private[data] trait ValidatedFunctions {

  /**
   * Converts an `E` to a `Validated[E, A]`.
   *
   * For example:
   * {{{
   * scala> Validated.invalid[IllegalArgumentException, String](new IllegalArgumentException("Argument is nonzero"))
   * res0: Validated[IllegalArgumentException, String] = Invalid(java.lang.IllegalArgumentException: Argument is nonzero)
   * }}}
   */
  def invalid[E, A](e: E): Validated[E, A] = Validated.Invalid(e)

  /**
   * Converts an `E` to a `ValidatedNel[E, A]`.
   *
   * For example:
   * {{{
   * scala> Validated.invalidNel[IllegalArgumentException, String](new IllegalArgumentException("Argument is nonzero"))
   * res0: ValidatedNel[IllegalArgumentException, String] = Invalid(NonEmptyList(java.lang.IllegalArgumentException: Argument is nonzero))
   * }}}
   */
  def invalidNel[E, A](e: E): ValidatedNel[E, A] = Validated.Invalid(NonEmptyList(e, Nil))

  /**
   * Converts a `A` to a `Validated[E, A]`.
   *
   * For example:
   * {{{
   * scala> Validated.valid[IllegalArgumentException, String]("Hello world")
   * res0: Validated[IllegalArgumentException, String] = Valid(Hello world)
   * }}}
   */
  def valid[E, A](a: A): Validated[E, A] = Validated.Valid(a)

  /**
   * Converts a `A` to a `ValidatedNel[E, A]`.
   *
   * For example:
   * {{{
   * scala> Validated.validNel[IllegalArgumentException, String]("Hello world")
   * res0: ValidatedNel[IllegalArgumentException, String] = Valid(Hello world)
   * }}}
   */
  def validNel[E, A](a: A): ValidatedNel[E, A] = Validated.Valid(a)

  def catchNonFatal[A](f: => A): Validated[Throwable, A] =
    try {
      valid(f)
    } catch {
      case t if scala.util.control.NonFatal(t) => invalid(t)
    }

  /**
   * Converts a `Try[A]` to a `Validated[Throwable, A]`.
   */
  def fromTry[A](t: Try[A]): Validated[Throwable, A] =
    t match {
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

  /**
   * If the condition is satisfied, return the given `A` as valid,
   * otherwise return the given `E` as invalid.
   */
  final def cond[E, A](test: Boolean, a: => A, e: => E): Validated[E, A] =
    if (test) valid(a) else invalid(e)

  /**
   * If the condition is satisfied, return the given `A` as valid NEL,
   * otherwise return the given `E` as invalid NEL.
   */
  final def condNel[E, A](test: Boolean, a: => A, e: => E): ValidatedNel[E, A] =
    if (test) validNel(a) else invalidNel(e)
}

private[data] trait ValidatedFunctionsBinCompat0 {

  /**
   * Converts a `B` to a `ValidatedNec[A, B]`.
   *
   * For example:
   * {{{
   * scala> Validated.validNec[IllegalArgumentException, String]("Hello world")
   * res0: ValidatedNec[IllegalArgumentException, String] = Valid(Hello world)
   * }}}
   */
  def validNec[A, B](b: B): ValidatedNec[A, B] = Validated.Valid(b)

  /**
   * Converts an `A` to a `ValidatedNec[A, B]`.
   *
   * For example:
   * {{{
   * scala> Validated.invalidNec[IllegalArgumentException, String](new IllegalArgumentException("Argument is nonzero"))
   * res0: ValidatedNec[IllegalArgumentException, String] = Invalid(Chain(java.lang.IllegalArgumentException: Argument is nonzero))
   * }}}
   */
  def invalidNec[A, B](a: A): ValidatedNec[A, B] = Validated.Invalid(NonEmptyChain.one(a))

  /**
   * If the condition is satisfied, return the given `B` as valid NEC,
   * otherwise return the given `A` as invalid NEC.
   */
  final def condNec[A, B](test: Boolean, b: => B, a: => A): ValidatedNec[A, B] =
    if (test) validNec(b) else invalidNec(a)
}
